<?php
/**
 * Real Estate Properties Controller
 * Handles mobile property capture and COBOL payment processing
 */

if (!defined('sugarEntry') || !sugarEntry) {
    die('Not A Valid Entry Point');
}

require_once('include/MVC/Controller/SugarController.php');
require_once('modules/RE_Properties/RE_Properties.php');

class RE_PropertiesController extends SugarController
{
    /**
     * Handle mobile property quick capture
     * Processes images, GPS data, and voice notes from mobile devices
     */
    public function action_QuickCapture()
    {
        global $current_user, $db;
        
        // Check authentication
        if (!$current_user->id) {
            $this->returnJsonError('Authentication required', 401);
            return;
        }
        
        try {
            // Get JSON input data
            $inputData = $this->getJsonInput();
            
            // Create new property bean
            $property = BeanFactory::newBean('RE_Properties');
            
            // Basic property data
            $property->name = $inputData['name'] ?? 'Mobile Capture ' . date('Y-m-d H:i:s');
            $property->property_type = $inputData['property_type'] ?? 'residential';
            $property->listing_price = floatval($inputData['listing_price'] ?? 0);
            $property->bedrooms = intval($inputData['bedrooms'] ?? 0);
            $property->bathrooms = floatval($inputData['bathrooms'] ?? 0);
            $property->square_feet = intval($inputData['square_feet'] ?? 0);
            $property->property_status = 'draft';
            $property->assigned_user_id = $current_user->id;
            
            // Process GPS coordinates
            if (!empty($inputData['gps_latitude']) && !empty($inputData['gps_longitude'])) {
                $property->latitude = floatval($inputData['gps_latitude']);
                $property->longitude = floatval($inputData['gps_longitude']);
                
                // Mock reverse geocoding (in production, would call real service)
                if ($property->latitude && $property->longitude) {
                    $property->address_street = $inputData['address_street'] ?? '123 Main Street';
                    $property->address_city = $inputData['address_city'] ?? 'Springfield';
                    $property->address_state = $inputData['address_state'] ?? 'IL';
                    $property->address_postalcode = $inputData['address_postalcode'] ?? '62701';
                }
            }
            
            // Process image data
            if (!empty($inputData['image_data'])) {
                $uploadDir = 'upload/properties/' . date('Y/m/');
                if (!file_exists($uploadDir)) {
                    mkdir($uploadDir, 0777, true);
                }
                
                $filename = uniqid('property_') . '.jpg';
                $filepath = $uploadDir . $filename;
                
                // Decode base64 image
                $imageData = $inputData['image_data'];
                $imageData = preg_replace('#^data:image/\w+;base64,#i', '', $imageData);
                $image = base64_decode($imageData);
                
                if ($image !== false) {
                    file_put_contents($filepath, $image);
                    $property->property_images = $filepath;
                }
            }
            
            // Process voice note (mock transcription)
            if (!empty($inputData['voice_note'])) {
                $property->description = "Voice note captured: " . substr($inputData['voice_note'], 0, 100) . "...";
            }
            
            // Mobile capture metadata
            $property->captured_mobile = true;
            $property->capture_timestamp = date('Y-m-d H:i:s');
            $property->capture_device = $_SERVER['HTTP_USER_AGENT'] ?? 'Unknown Device';
            
            // Save the property
            $property->save();
            
            // Return success response
            $this->returnJsonSuccess([
                'id' => $property->id,
                'name' => $property->name,
                'status' => 'Property captured successfully',
                'property_url' => 'index.php?module=RE_Properties&action=DetailView&record=' . $property->id,
                'mobile_url' => 'index.php?module=RE_Properties&action=Mobile&record=' . $property->id
            ]);
            
        } catch (Exception $e) {
            $GLOBALS['log']->error('Quick Capture Error: ' . $e->getMessage());
            $this->returnJsonError('Failed to capture property: ' . $e->getMessage());
        }
    }
    
    /**
     * Process COBOL payment for earnest money
     */
    public function action_ProcessPayment()
    {
        global $current_user;
        
        // Check authentication
        if (!$current_user->id) {
            $this->returnJsonError('Authentication required', 401);
            return;
        }
        
        try {
            // Get payment data
            $inputData = $this->getJsonInput();
            
            $cardNumber = $inputData['number'] ?? '';
            $expiry = $inputData['expiry'] ?? '';
            $cvv = $inputData['cvv'] ?? '';
            $propertyId = $inputData['property_id'] ?? '';
            $amount = floatval($inputData['amount'] ?? 0);
            
            // Validate input
            if (empty($cardNumber) || empty($expiry) || empty($cvv) || empty($propertyId)) {
                $this->returnJsonError('Missing required payment information');
                return;
            }
            
            // Load property
            $property = BeanFactory::getBean('RE_Properties', $propertyId);
            if (!$property || !$property->id) {
                $this->returnJsonError('Property not found');
                return;
            }
            
            // Mock COBOL payment processing
            // In production, this would call actual COBOL service
            $transactionId = 'COBOL-' . strtoupper(uniqid());
            $authCode = strtoupper(substr(md5($cardNumber . time()), 0, 6));
            
            // Simulate validation (demo always approves if card starts with 4)
            $isApproved = substr($cardNumber, 0, 1) === '4';
            
            if ($isApproved) {
                // Update property with payment info
                $property->cobol_validation_status = 'APPROVED';
                $property->cobol_transaction_id = $transactionId;
                $property->earnest_money_received = true;
                $property->earnest_money_date = date('Y-m-d');
                $property->property_status = 'under_contract';
                $property->payment_processor_response = json_encode([
                    'status' => 'APPROVED',
                    'transaction_id' => $transactionId,
                    'auth_code' => $authCode,
                    'timestamp' => date('Y-m-d H:i:s')
                ]);
                $property->save();
                
                $this->returnJsonSuccess([
                    'status' => 'APPROVED',
                    'transaction_id' => $transactionId,
                    'auth_code' => $authCode,
                    'message' => 'Payment approved successfully',
                    'property_status' => 'under_contract'
                ]);
            } else {
                $this->returnJsonError('Payment declined', 400, [
                    'status' => 'DECLINED',
                    'message' => 'Card validation failed',
                    'decline_code' => 'INVALID_CARD'
                ]);
            }
            
        } catch (Exception $e) {
            $GLOBALS['log']->error('Payment Processing Error: ' . $e->getMessage());
            $this->returnJsonError('Payment processing failed: ' . $e->getMessage());
        }
    }
    
    /**
     * Mobile-optimized property list
     */
    public function action_MobileList()
    {
        global $current_user, $db;
        
        // Check authentication
        if (!$current_user->id) {
            $this->returnJsonError('Authentication required', 401);
            return;
        }
        
        try {
            // Get request parameters
            $offset = intval($_REQUEST['offset'] ?? 0);
            $limit = intval($_REQUEST['limit'] ?? 20);
            $search = $_REQUEST['search'] ?? '';
            $status = $_REQUEST['status'] ?? '';
            $userLat = floatval($_REQUEST['user_lat'] ?? 0);
            $userLng = floatval($_REQUEST['user_lng'] ?? 0);
            $radius = floatval($_REQUEST['radius'] ?? 50); // Default 50km radius
            
            // Build query
            $where = "re_properties.deleted = 0";
            
            if (!empty($search)) {
                $searchTerm = $db->quote('%' . $search . '%');
                $where .= " AND (re_properties.name LIKE {$searchTerm} 
                           OR re_properties.address_street LIKE {$searchTerm}
                           OR re_properties.address_city LIKE {$searchTerm})";
            }
            
            if (!empty($status)) {
                $where .= " AND re_properties.property_status = " . $db->quoted($status);
            }
            
            // Base query
            $query = "SELECT 
                        re_properties.id,
                        re_properties.name,
                        re_properties.listing_price,
                        re_properties.property_status,
                        re_properties.bedrooms,
                        re_properties.bathrooms,
                        re_properties.square_feet,
                        re_properties.address_city,
                        re_properties.address_state,
                        re_properties.property_images,
                        re_properties.latitude,
                        re_properties.longitude";
            
            // Add distance calculation if coordinates provided
            if ($userLat && $userLng) {
                $query .= ", (6371 * acos(cos(radians({$userLat})) * cos(radians(latitude)) * 
                          cos(radians(longitude) - radians({$userLng})) + sin(radians({$userLat})) * 
                          sin(radians(latitude)))) AS distance";
            }
            
            $query .= " FROM re_properties WHERE {$where}";
            
            // Filter by radius if coordinates provided
            if ($userLat && $userLng) {
                $query .= " HAVING distance <= {$radius}";
                $query .= " ORDER BY distance ASC";
            } else {
                $query .= " ORDER BY re_properties.date_modified DESC";
            }
            
            $query .= " LIMIT {$limit} OFFSET {$offset}";
            
            // Execute query
            $result = $db->query($query);
            $properties = [];
            
            while ($row = $db->fetchByAssoc($result)) {
                // Format price
                $row['listing_price_formatted'] = '$' . number_format($row['listing_price'], 0);
                
                // Add thumbnail if image exists
                if (!empty($row['property_images'])) {
                    $row['thumbnail'] = $row['property_images'];
                } else {
                    $row['thumbnail'] = 'themes/default/images/no-property-image.png';
                }
                
                // Format distance
                if (isset($row['distance'])) {
                    $row['distance_formatted'] = number_format($row['distance'], 1) . ' km';
                }
                
                $properties[] = $row;
            }
            
            // Get total count
            $countQuery = "SELECT COUNT(*) as total FROM re_properties WHERE {$where}";
            $countResult = $db->query($countQuery);
            $countRow = $db->fetchByAssoc($countResult);
            $totalCount = intval($countRow['total'] ?? 0);
            
            $this->returnJsonSuccess([
                'properties' => $properties,
                'total' => $totalCount,
                'offset' => $offset,
                'limit' => $limit,
                'has_more' => ($offset + $limit) < $totalCount
            ]);
            
        } catch (Exception $e) {
            $GLOBALS['log']->error('Mobile List Error: ' . $e->getMessage());
            $this->returnJsonError('Failed to load properties: ' . $e->getMessage());
        }
    }
    
    /**
     * Handle offline sync
     */
    public function action_SyncOffline()
    {
        global $current_user;
        
        // Check authentication
        if (!$current_user->id) {
            $this->returnJsonError('Authentication required', 401);
            return;
        }
        
        try {
            // Get sync data
            $inputData = $this->getJsonInput();
            
            $lastSyncTime = $inputData['last_sync'] ?? null;
            $localChanges = $inputData['local_changes'] ?? [];
            $deviceId = $inputData['device_id'] ?? 'unknown';
            
            $syncResults = [
                'synced' => [],
                'conflicts' => [],
                'errors' => []
            ];
            
            // Process local changes
            foreach ($localChanges as $change) {
                try {
                    $action = $change['action'] ?? '';
                    $data = $change['data'] ?? [];
                    $localId = $change['local_id'] ?? '';
                    
                    switch ($action) {
                        case 'create':
                            // Create new property
                            $property = BeanFactory::newBean('RE_Properties');
                            $this->populatePropertyFromData($property, $data);
                            $property->sync_device_id = $deviceId;
                            $property->save();
                            
                            $syncResults['synced'][] = [
                                'local_id' => $localId,
                                'server_id' => $property->id,
                                'action' => 'created'
                            ];
                            break;
                            
                        case 'update':
                            // Update existing property
                            if (!empty($data['id'])) {
                                $property = BeanFactory::getBean('RE_Properties', $data['id']);
                                if ($property && $property->id) {
                                    // Check for conflicts
                                    if ($property->date_modified > $data['last_modified']) {
                                        $syncResults['conflicts'][] = [
                                            'id' => $property->id,
                                            'local_modified' => $data['last_modified'],
                                            'server_modified' => $property->date_modified
                                        ];
                                    } else {
                                        $this->populatePropertyFromData($property, $data);
                                        $property->save();
                                        
                                        $syncResults['synced'][] = [
                                            'id' => $property->id,
                                            'action' => 'updated'
                                        ];
                                    }
                                }
                            }
                            break;
                            
                        case 'delete':
                            // Delete property
                            if (!empty($data['id'])) {
                                $property = BeanFactory::getBean('RE_Properties', $data['id']);
                                if ($property && $property->id) {
                                    $property->mark_deleted($property->id);
                                    
                                    $syncResults['synced'][] = [
                                        'id' => $property->id,
                                        'action' => 'deleted'
                                    ];
                                }
                            }
                            break;
                    }
                } catch (Exception $e) {
                    $syncResults['errors'][] = [
                        'local_id' => $localId,
                        'error' => $e->getMessage()
                    ];
                }
            }
            
            // Get server changes since last sync
            $serverChanges = [];
            if ($lastSyncTime) {
                $where = "re_properties.deleted = 0 AND re_properties.date_modified > " . 
                        $GLOBALS['db']->quoted($lastSyncTime);
                
                $query = "SELECT id, name, listing_price, property_status, bedrooms, bathrooms, 
                         square_feet, address_city, address_state, property_images, 
                         latitude, longitude, date_modified
                         FROM re_properties 
                         WHERE {$where}
                         ORDER BY date_modified DESC
                         LIMIT 100";
                
                $result = $GLOBALS['db']->query($query);
                while ($row = $GLOBALS['db']->fetchByAssoc($result)) {
                    $serverChanges[] = $row;
                }
            }
            
            $this->returnJsonSuccess([
                'sync_time' => date('Y-m-d H:i:s'),
                'device_id' => $deviceId,
                'sync_results' => $syncResults,
                'server_changes' => $serverChanges,
                'success' => true
            ]);
            
        } catch (Exception $e) {
            $GLOBALS['log']->error('Sync Error: ' . $e->getMessage());
            $this->returnJsonError('Sync failed: ' . $e->getMessage());
        }
    }
    
    /**
     * Helper method to populate property from data array
     */
    private function populatePropertyFromData($property, $data)
    {
        $fields = [
            'name', 'property_type', 'listing_price', 'bedrooms', 'bathrooms',
            'square_feet', 'lot_size', 'year_built', 'property_status', 'mls_number',
            'address_street', 'address_city', 'address_state', 'address_postalcode',
            'latitude', 'longitude', 'property_features', 'commission_rate',
            'earnest_money', 'closing_date', 'description'
        ];
        
        foreach ($fields as $field) {
            if (isset($data[$field])) {
                $property->$field = $data[$field];
            }
        }
    }
    
    /**
     * Get JSON input data
     */
    private function getJsonInput()
    {
        $input = file_get_contents('php://input');
        if (!empty($input)) {
            return json_decode($input, true) ?? [];
        }
        return $_REQUEST;
    }
    
    /**
     * Return JSON success response
     */
    private function returnJsonSuccess($data = [])
    {
        ob_clean();
        header('Content-Type: application/json');
        echo json_encode(array_merge(['success' => true], $data));
        exit;
    }
    
    /**
     * Return JSON error response
     */
    private function returnJsonError($message, $code = 400, $additionalData = [])
    {
        ob_clean();
        header('Content-Type: application/json');
        http_response_code($code);
        echo json_encode(array_merge([
            'success' => false,
            'error' => $message
        ], $additionalData));
        exit;
    }
}