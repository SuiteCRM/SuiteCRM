<?php
/**
 * Mobile View for Real Estate Properties
 * Optimized for real estate agents on the go
 */

require_once('include/MVC/View/SugarView.php');

class RE_PropertiesViewMobile extends SugarView
{
    public function __construct()
    {
        parent::__construct();
        $this->options['show_header'] = false;
        $this->options['show_footer'] = false;
        $this->options['show_javascript'] = true;
    }
    
    public function display()
    {
        // Set mobile theme
        $GLOBALS['theme'] = 'MobileRealEstate';
        
        // Get user location if available
        $userLat = $_REQUEST['lat'] ?? null;
        $userLng = $_REQUEST['lng'] ?? null;
        
        // Initialize COBOL service
        $cobolService = new COBOLPaymentService();
        $cobolStatus = $cobolService->getSystemStatus();
        
        // Pass data to template
        $this->ss->assign('USER_LAT', $userLat);
        $this->ss->assign('USER_LNG', $userLng);
        $this->ss->assign('COBOL_STATUS', $cobolStatus);
        $this->ss->assign('PROPERTY', $this->bean);
        
        // Include PWA manifest
        $this->_displayPWAHeaders();
        
        // Display mobile template
        echo $this->_displayMobileTemplate();
    }
    
    private function _displayPWAHeaders()
    {
        echo <<<HTML
<!DOCTYPE html>
<html lang="en">
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0, maximum-scale=1.0, user-scalable=no">
    <meta name="apple-mobile-web-app-capable" content="yes">
    <meta name="apple-mobile-web-app-status-bar-style" content="default">
    <meta name="theme-color" content="#2c5aa0">
    
    <title>SuiteCRM Real Estate Mobile</title>
    
    <link rel="manifest" href="themes/MobileRealEstate/manifest.json">
    <link rel="apple-touch-icon" href="themes/MobileRealEstate/images/icon-192.png">
    <link rel="stylesheet" href="themes/MobileRealEstate/css/mobile-realestate.css">
    
    <script src="include/javascript/jquery/jquery-min.js"></script>
    <script src="themes/MobileRealEstate/js/mobile-app.js"></script>
</head>
<body class="mobile-view">
HTML;
    }
    
    private function _displayMobileTemplate()
    {
        ob_start();
        ?>
        <!-- Mobile Header -->
        <header class="mobile-header">
            <div class="menu-toggle" onclick="toggleMobileNav()">
                <div class="hamburger"></div>
            </div>
            <div class="header-title">Properties</div>
            <div class="quick-actions">
                <div class="action-btn" onclick="showNotifications()">
                    <i class="fa fa-bell"></i>
                </div>
                <div class="action-btn" onclick="syncData()">
                    <i class="fa fa-sync"></i>
                </div>
            </div>
        </header>
        
        <!-- Mobile Navigation -->
        <nav class="mobile-nav" id="mobileNav">
            <a href="#" class="nav-item" onclick="navigateTo('properties')">
                <i class="fa fa-home"></i>
                <span>Properties</span>
            </a>
            <a href="#" class="nav-item" onclick="navigateTo('clients')">
                <i class="fa fa-users"></i>
                <span>Clients</span>
            </a>
            <a href="#" class="nav-item" onclick="navigateTo('appointments')">
                <i class="fa fa-calendar"></i>
                <span>Appointments</span>
            </a>
            <a href="#" class="nav-item" onclick="navigateTo('messages')">
                <i class="fa fa-envelope"></i>
                <span>Messages</span>
            </a>
            <a href="#" class="nav-item" onclick="navigateTo('analytics')">
                <i class="fa fa-chart-bar"></i>
                <span>Analytics</span>
            </a>
            <a href="#" class="nav-item" onclick="navigateTo('settings')">
                <i class="fa fa-cog"></i>
                <span>Settings</span>
            </a>
        </nav>
        
        <!-- Main Content -->
        <main class="mobile-content" style="margin-top: 60px;">
            <?php if ($this->bean->id): ?>
                <!-- Single Property View -->
                <div class="property-detail">
                    <?php if ($this->bean->property_images): ?>
                        <div class="property-gallery">
                            <img src="<?php echo $this->bean->property_images; ?>" alt="Property Image">
                            <?php if ($this->bean->virtual_tour_url): ?>
                                <button class="virtual-tour-btn" onclick="launch360Tour('<?php echo $this->bean->virtual_tour_url; ?>')">
                                    <i class="fa fa-vr-cardboard"></i> Virtual Tour
                                </button>
                            <?php endif; ?>
                        </div>
                    <?php endif; ?>
                    
                    <div class="property-info">
                        <h1><?php echo $this->bean->name; ?></h1>
                        <div class="property-price">$<?php echo number_format($this->bean->listing_price, 0); ?></div>
                        <div class="property-address">
                            <i class="fa fa-map-marker-alt"></i>
                            <?php echo $this->bean->address_street; ?>, 
                            <?php echo $this->bean->address_city; ?>, 
                            <?php echo $this->bean->address_state; ?> 
                            <?php echo $this->bean->address_postalcode; ?>
                        </div>
                        
                        <div class="property-features">
                            <div class="feature">
                                <i class="fa fa-bed"></i>
                                <span><?php echo $this->bean->bedrooms; ?> Beds</span>
                            </div>
                            <div class="feature">
                                <i class="fa fa-bath"></i>
                                <span><?php echo $this->bean->bathrooms; ?> Baths</span>
                            </div>
                            <div class="feature">
                                <i class="fa fa-ruler-combined"></i>
                                <span><?php echo number_format($this->bean->square_feet); ?> sq ft</span>
                            </div>
                        </div>
                        
                        <!-- COBOL Payment Integration -->
                        <?php if ($this->bean->property_status == 'available' && $GLOBALS['cobol_status']['online']): ?>
                            <div class="cobol-payment-widget">
                                <h3>Process Earnest Money <span class="cobol-badge">COBOL</span></h3>
                                <div class="earnest-amount">
                                    Required: $<?php echo number_format($this->bean->earnest_money, 2); ?>
                                </div>
                                
                                <form id="paymentForm" onsubmit="processPayment(event)">
                                    <div class="card-input">
                                        <input type="text" id="cardNumber" placeholder="Card Number" maxlength="19" required>
                                        <i class="fa fa-credit-card card-icon"></i>
                                    </div>
                                    
                                    <div style="display: flex; gap: 10px;">
                                        <div class="card-input" style="flex: 1;">
                                            <input type="text" id="expiry" placeholder="MM/YY" maxlength="5" required>
                                        </div>
                                        <div class="card-input" style="flex: 1;">
                                            <input type="text" id="cvv" placeholder="CVV" maxlength="3" required>
                                        </div>
                                    </div>
                                    
                                    <button type="submit" class="process-payment-btn">
                                        <i class="fa fa-lock"></i> Process Payment
                                    </button>
                                </form>
                                
                                <div id="validationStatus" class="validation-status" style="display: none;">
                                    <div class="spinner"></div>
                                    <span>Validating with COBOL system...</span>
                                </div>
                            </div>
                        <?php endif; ?>
                        
                        <!-- Quick Actions -->
                        <div class="property-quick-actions">
                            <button onclick="scheduleShowing('<?php echo $this->bean->id; ?>')">
                                <i class="fa fa-calendar-plus"></i>
                                Schedule Showing
                            </button>
                            <button onclick="shareProperty('<?php echo $this->bean->id; ?>')">
                                <i class="fa fa-share-alt"></i>
                                Share Property
                            </button>
                            <button onclick="generateQRCode('<?php echo $this->bean->id; ?>')">
                                <i class="fa fa-qrcode"></i>
                                Generate QR
                            </button>
                        </div>
                    </div>
                </div>
            <?php else: ?>
                <!-- Property List View -->
                <div class="property-search">
                    <input type="search" placeholder="Search properties..." onkeyup="searchProperties(this.value)">
                    <button onclick="toggleFilters()">
                        <i class="fa fa-filter"></i>
                    </button>
                </div>
                
                <div class="mobile-grid" id="propertyGrid">
                    <!-- Properties will be loaded here via AJAX -->
                </div>
            <?php endif; ?>
        </main>
        
        <!-- Property Quick Capture FAB -->
        <div class="property-capture">
            <div class="capture-fab" onclick="toggleCaptureOptions()">
                <i class="fa fa-plus"></i>
            </div>
            <div class="capture-options" id="captureOptions">
                <div class="option camera" onclick="capturePhoto()">
                    <i class="fa fa-camera"></i>
                </div>
                <div class="option voice" onclick="recordVoiceNote()">
                    <i class="fa fa-microphone"></i>
                </div>
                <div class="option location" onclick="captureLocation()">
                    <i class="fa fa-map-pin"></i>
                </div>
                <div class="option qr" onclick="scanQRCode()">
                    <i class="fa fa-qrcode"></i>
                </div>
            </div>
        </div>
        
        <!-- Offline Indicator -->
        <div class="offline-indicator" id="offlineIndicator">
            <i class="fa fa-wifi-slash"></i> Offline Mode - Changes will sync when connected
        </div>
        
        <!-- PWA Install Prompt -->
        <div class="pwa-install-prompt" id="pwaPrompt">
            <div class="prompt-content">
                <div class="app-icon">
                    <i class="fa fa-home"></i>
                </div>
                <div class="prompt-text">
                    <h4>Install SuiteCRM Mobile</h4>
                    <p>Add to your home screen for the best experience</p>
                </div>
                <button class="install-btn" onclick="installPWA()">Install</button>
            </div>
        </div>
        
        <!-- COBOL WebSocket Connection -->
        <?php echo $cobolService->connectWebSocket($this->bean->id); ?>
        
        <script>
            // Initialize mobile app
            document.addEventListener('DOMContentLoaded', function() {
                initializeMobileApp();
                checkOnlineStatus();
                setupPWA();
                
                <?php if (!$this->bean->id): ?>
                    loadProperties();
                <?php endif; ?>
            });
            
            // COBOL Payment Processing
            function processPayment(event) {
                event.preventDefault();
                
                const validationStatus = document.getElementById('validationStatus');
                validationStatus.style.display = 'flex';
                validationStatus.className = 'validation-status';
                
                const cardData = {
                    number: document.getElementById('cardNumber').value,
                    expiry: document.getElementById('expiry').value,
                    cvv: document.getElementById('cvv').value,
                    property_id: '<?php echo $this->bean->id; ?>',
                    amount: <?php echo $this->bean->earnest_money; ?>
                };
                
                // Send to COBOL via WebSocket
                if (window.cobolWebSocket && window.cobolWebSocket.readyState === WebSocket.OPEN) {
                    window.cobolWebSocket.send(JSON.stringify({
                        action: 'validate_payment',
                        data: cardData
                    }));
                } else {
                    // Fallback to AJAX
                    $.ajax({
                        url: 'index.php?module=RE_Properties&action=ProcessPayment',
                        method: 'POST',
                        data: cardData,
                        success: function(response) {
                            handlePaymentResponse(response);
                        },
                        error: function() {
                            validationStatus.className = 'validation-status invalid';
                            validationStatus.innerHTML = '<i class="fa fa-times-circle"></i> Payment processing failed';
                        }
                    });
                }
            }
            
            // Handle COBOL payment response
            function handlePaymentResponse(response) {
                const validationStatus = document.getElementById('validationStatus');
                
                if (response.status === 'APPROVED') {
                    validationStatus.className = 'validation-status valid';
                    validationStatus.innerHTML = '<i class="fa fa-check-circle"></i> Payment approved! Auth: ' + response.auth_code;
                    
                    // Update UI
                    setTimeout(() => {
                        window.location.reload();
                    }, 2000);
                } else {
                    validationStatus.className = 'validation-status invalid';
                    validationStatus.innerHTML = '<i class="fa fa-times-circle"></i> ' + response.message;
                }
            }
        </script>
        
        </body>
        </html>
        <?php
        return ob_get_clean();
    }
}