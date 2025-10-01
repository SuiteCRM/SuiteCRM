<?php
/**
 * Real Estate Properties Module
 * Mobile-optimized property management with COBOL integration
 */

class RE_Properties extends Basic
{
    public $module_dir = 'RE_Properties';
    public $object_name = 'RE_Properties';
    public $table_name = 're_properties';
    public $new_schema = true;
    public $module_name = 'RE_Properties';
    
    // Property specific fields
    public $name;
    public $property_type;
    public $listing_price;
    public $bedrooms;
    public $bathrooms;
    public $square_feet;
    public $lot_size;
    public $year_built;
    public $property_status;
    public $mls_number;
    public $address_street;
    public $address_city;
    public $address_state;
    public $address_postalcode;
    public $latitude;
    public $longitude;
    public $property_images;
    public $virtual_tour_url;
    public $property_features;
    public $listing_agent_id;
    public $commission_rate;
    public $earnest_money;
    public $closing_date;
    
    // COBOL integration fields
    public $cobol_validation_status;
    public $cobol_transaction_id;
    public $payment_processor_response;
    
    public function __construct()
    {
        parent::__construct();
        $this->disable_row_level_security = false;
        $this->setupCustomFields('RE_Properties');
    }
    
    /**
     * Quick capture property from mobile device
     */
    public function quickCapture($data)
    {
        // Process image if captured from camera
        if (!empty($data['image_data'])) {
            $this->property_images = $this->processPropertyImage($data['image_data']);
        }
        
        // Get GPS coordinates if available
        if (!empty($data['gps_latitude']) && !empty($data['gps_longitude'])) {
            $this->latitude = $data['gps_latitude'];
            $this->longitude = $data['gps_longitude'];
            
            // Reverse geocode to get address
            $address = $this->reverseGeocode($this->latitude, $this->longitude);
            if ($address) {
                $this->address_street = $address['street'];
                $this->address_city = $address['city'];
                $this->address_state = $address['state'];
                $this->address_postalcode = $address['postalcode'];
            }
        }
        
        // Process voice notes if available
        if (!empty($data['voice_note'])) {
            $this->description = $this->transcribeVoiceNote($data['voice_note']);
        }
        
        // Set mobile capture flag
        $this->captured_mobile = true;
        $this->capture_timestamp = date('Y-m-d H:i:s');
        
        return $this->save();
    }
    
    /**
     * Process property image from mobile camera
     */
    private function processPropertyImage($imageData)
    {
        $upload_dir = 'upload/properties/' . date('Y/m/');
        if (!file_exists($upload_dir)) {
            mkdir($upload_dir, 0777, true);
        }
        
        $filename = uniqid('property_') . '.jpg';
        $filepath = $upload_dir . $filename;
        
        // Decode base64 image data
        $image = base64_decode(preg_replace('#^data:image/\w+;base64,#i', '', $imageData));
        file_put_contents($filepath, $image);
        
        // Generate thumbnail
        $this->generateThumbnail($filepath);
        
        // Extract EXIF data if available
        $exif = @exif_read_data($filepath);
        if ($exif) {
            $this->image_metadata = json_encode($exif);
        }
        
        return $filepath;
    }
    
    /**
     * COBOL credit card validation
     */
    public function validatePayment($cardData)
    {
        global $sugar_config;
        
        // Prepare COBOL request
        $cobolRequest = array(
            'card_number' => $cardData['number'],
            'expiry_month' => $cardData['exp_month'],
            'expiry_year' => $cardData['exp_year'],
            'cvv' => $cardData['cvv'],
            'amount' => $this->earnest_money,
            'property_id' => $this->id,
            'transaction_type' => 'EARNEST_DEPOSIT'
        );
        
        // Call COBOL validation service
        $cobolService = new COBOLPaymentService();
        $response = $cobolService->validateCard($cobolRequest);
        
        // Store validation result
        $this->cobol_validation_status = $response['status'];
        $this->cobol_transaction_id = $response['transaction_id'];
        $this->payment_processor_response = json_encode($response);
        
        // Update property status if payment validated
        if ($response['status'] === 'APPROVED') {
            $this->property_status = 'under_contract';
            $this->earnest_money_received = true;
            $this->earnest_money_date = date('Y-m-d');
        }
        
        $this->save();
        
        return $response;
    }
    
    /**
     * Generate property QR code
     */
    public function generateQRCode()
    {
        require_once('include/phpqrcode/qrlib.php');
        
        $propertyUrl = $GLOBALS['sugar_config']['site_url'] . '/index.php?module=RE_Properties&action=MobileView&record=' . $this->id;
        $qrFile = 'upload/qrcodes/property_' . $this->id . '.png';
        
        if (!file_exists('upload/qrcodes')) {
            mkdir('upload/qrcodes', 0777, true);
        }
        
        QRcode::png($propertyUrl, $qrFile, QR_ECLEVEL_L, 10);
        
        $this->qr_code_path = $qrFile;
        $this->save();
        
        return $qrFile;
    }
    
    /**
     * Calculate property metrics
     */
    public function calculateMetrics()
    {
        // Price per square foot
        if ($this->square_feet > 0) {
            $this->price_per_sqft = $this->listing_price / $this->square_feet;
        }
        
        // Estimated monthly payment (using COBOL calculation service)
        $cobolCalc = new COBOLFinancialCalculator();
        $this->estimated_payment = $cobolCalc->calculateMortgage(
            $this->listing_price,
            0.20, // 20% down payment
            30, // 30 year term
            $GLOBALS['sugar_config']['current_mortgage_rate'] ?? 7.0
        );
        
        // Commission calculation
        if ($this->commission_rate > 0) {
            $this->commission_amount = $this->listing_price * ($this->commission_rate / 100);
        }
        
        return true;
    }
    
    /**
     * Mobile-optimized list query
     */
    public function create_list_query($order_by, $where, $filter = array(), $params = array(), $show_deleted = 0, $join_type = '', $return_array = false, $parentbean = null, $singleSelect = false, $ifListForExport = false)
    {
        // Add mobile-specific optimizations
        if (!empty($_REQUEST['mobile']) || $this->isMobileRequest()) {
            // Limit fields for mobile performance
            $params['select'] = 'SELECT re_properties.id, re_properties.name, re_properties.listing_price, 
                                re_properties.property_status, re_properties.bedrooms, re_properties.bathrooms,
                                re_properties.square_feet, re_properties.address_city, re_properties.property_images';
            
            // Add distance calculation if location provided
            if (!empty($_REQUEST['user_lat']) && !empty($_REQUEST['user_lng'])) {
                $userLat = $GLOBALS['db']->quote($_REQUEST['user_lat']);
                $userLng = $GLOBALS['db']->quote($_REQUEST['user_lng']);
                
                $params['select'] .= ", (6371 * acos(cos(radians($userLat)) * cos(radians(latitude)) * 
                                      cos(radians(longitude) - radians($userLng)) + sin(radians($userLat)) * 
                                      sin(radians(latitude)))) AS distance";
                
                $params['order_by'] = 'distance ASC';
            }
        }
        
        return parent::create_list_query($order_by, $where, $filter, $params, $show_deleted, $join_type, $return_array, $parentbean, $singleSelect, $ifListForExport);
    }
    
    /**
     * Check if request is from mobile device
     */
    private function isMobileRequest()
    {
        $userAgent = $_SERVER['HTTP_USER_AGENT'] ?? '';
        return preg_match('/(android|iphone|ipad|mobile)/i', $userAgent);
    }
}