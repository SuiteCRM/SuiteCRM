<?php
/**
 * Quick Capture Action View
 * Mobile-optimized property capture form
 */

if (!defined('sugarEntry') || !sugarEntry) {
    die('Not A Valid Entry Point');
}

require_once('include/MVC/View/SugarView.php');

class RE_PropertiesActionQuick_capture extends SugarView
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
        global $current_user;
        
        // Check authentication
        if (!$current_user->id) {
            header('Location: index.php?action=Login&module=Users');
            exit;
        }
        
        ?>
<!DOCTYPE html>
<html lang="en">
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0, maximum-scale=1.0, user-scalable=no">
    <meta name="apple-mobile-web-app-capable" content="yes">
    <title>Quick Property Capture</title>
    
    <style>
        * {
            margin: 0;
            padding: 0;
            box-sizing: border-box;
        }
        
        body {
            font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, sans-serif;
            background: #f5f5f5;
            color: #333;
            min-height: 100vh;
            overflow-x: hidden;
        }
        
        .capture-header {
            background: #2c5aa0;
            color: white;
            padding: 15px;
            display: flex;
            align-items: center;
            justify-content: space-between;
            position: fixed;
            top: 0;
            left: 0;
            right: 0;
            z-index: 1000;
            box-shadow: 0 2px 4px rgba(0,0,0,0.1);
        }
        
        .back-button {
            background: none;
            border: none;
            color: white;
            font-size: 24px;
            cursor: pointer;
            padding: 5px;
        }
        
        .capture-form {
            margin-top: 60px;
            padding: 20px;
            max-width: 600px;
            margin-left: auto;
            margin-right: auto;
        }
        
        .capture-section {
            background: white;
            border-radius: 12px;
            padding: 20px;
            margin-bottom: 20px;
            box-shadow: 0 2px 8px rgba(0,0,0,0.05);
        }
        
        .section-title {
            font-size: 18px;
            font-weight: 600;
            margin-bottom: 15px;
            color: #2c5aa0;
        }
        
        .capture-photo {
            width: 100%;
            height: 200px;
            background: #f0f0f0;
            border-radius: 8px;
            display: flex;
            align-items: center;
            justify-content: center;
            cursor: pointer;
            position: relative;
            overflow: hidden;
            margin-bottom: 15px;
        }
        
        .capture-photo img {
            width: 100%;
            height: 100%;
            object-fit: cover;
        }
        
        .capture-photo-placeholder {
            text-align: center;
            color: #666;
        }
        
        .capture-photo-placeholder i {
            font-size: 48px;
            margin-bottom: 10px;
            display: block;
        }
        
        #cameraInput {
            display: none;
        }
        
        .form-group {
            margin-bottom: 15px;
        }
        
        .form-label {
            display: block;
            font-size: 14px;
            font-weight: 500;
            margin-bottom: 5px;
            color: #555;
        }
        
        .form-control {
            width: 100%;
            padding: 12px;
            border: 1px solid #ddd;
            border-radius: 8px;
            font-size: 16px;
            transition: border-color 0.3s;
        }
        
        .form-control:focus {
            outline: none;
            border-color: #2c5aa0;
        }
        
        .form-row {
            display: grid;
            grid-template-columns: 1fr 1fr;
            gap: 15px;
        }
        
        .location-section {
            display: flex;
            align-items: center;
            gap: 15px;
            margin-bottom: 15px;
        }
        
        .location-button {
            background: #28a745;
            color: white;
            border: none;
            padding: 12px 20px;
            border-radius: 8px;
            font-size: 14px;
            font-weight: 500;
            cursor: pointer;
            display: flex;
            align-items: center;
            gap: 8px;
            white-space: nowrap;
        }
        
        .location-status {
            flex: 1;
            font-size: 14px;
            color: #666;
        }
        
        .voice-note-section {
            display: flex;
            align-items: center;
            gap: 15px;
        }
        
        .voice-button {
            background: #e83e8c;
            color: white;
            border: none;
            width: 60px;
            height: 60px;
            border-radius: 50%;
            font-size: 24px;
            cursor: pointer;
            display: flex;
            align-items: center;
            justify-content: center;
            transition: all 0.3s;
        }
        
        .voice-button.recording {
            background: #dc3545;
            animation: pulse 1.5s infinite;
        }
        
        @keyframes pulse {
            0% { transform: scale(1); }
            50% { transform: scale(1.1); }
            100% { transform: scale(1); }
        }
        
        .voice-status {
            flex: 1;
            font-size: 14px;
            color: #666;
        }
        
        .submit-section {
            margin-top: 30px;
            display: flex;
            gap: 15px;
        }
        
        .btn {
            flex: 1;
            padding: 15px;
            border: none;
            border-radius: 8px;
            font-size: 16px;
            font-weight: 600;
            cursor: pointer;
            transition: all 0.3s;
        }
        
        .btn-primary {
            background: #2c5aa0;
            color: white;
        }
        
        .btn-secondary {
            background: #6c757d;
            color: white;
        }
        
        .btn:hover {
            transform: translateY(-1px);
            box-shadow: 0 4px 12px rgba(0,0,0,0.15);
        }
        
        .loading-overlay {
            position: fixed;
            top: 0;
            left: 0;
            right: 0;
            bottom: 0;
            background: rgba(0,0,0,0.7);
            display: none;
            align-items: center;
            justify-content: center;
            z-index: 9999;
        }
        
        .loading-content {
            background: white;
            padding: 30px;
            border-radius: 12px;
            text-align: center;
        }
        
        .spinner {
            width: 50px;
            height: 50px;
            border: 5px solid #f3f3f3;
            border-top: 5px solid #2c5aa0;
            border-radius: 50%;
            animation: spin 1s linear infinite;
            margin: 0 auto 20px;
        }
        
        @keyframes spin {
            0% { transform: rotate(0deg); }
            100% { transform: rotate(360deg); }
        }
        
        .success-message {
            background: #d4edda;
            color: #155724;
            padding: 15px;
            border-radius: 8px;
            margin-bottom: 20px;
            display: none;
        }
        
        .error-message {
            background: #f8d7da;
            color: #721c24;
            padding: 15px;
            border-radius: 8px;
            margin-bottom: 20px;
            display: none;
        }
    </style>
</head>
<body>
    <!-- Header -->
    <header class="capture-header">
        <button class="back-button" onclick="goBack()">
            <i class="fa fa-arrow-left"></i>
        </button>
        <h1 style="font-size: 20px;">Quick Property Capture</h1>
        <div style="width: 34px;"></div>
    </header>
    
    <!-- Main Form -->
    <form id="quickCaptureForm" class="capture-form">
        <!-- Success/Error Messages -->
        <div id="successMessage" class="success-message"></div>
        <div id="errorMessage" class="error-message"></div>
        
        <!-- Photo Capture -->
        <div class="capture-section">
            <h2 class="section-title">
                <i class="fa fa-camera"></i> Property Photo
            </h2>
            <div class="capture-photo" onclick="document.getElementById('cameraInput').click()">
                <div class="capture-photo-placeholder" id="photoPlaceholder">
                    <i class="fa fa-camera"></i>
                    <div>Tap to capture photo</div>
                </div>
                <img id="photoPreview" style="display: none;">
            </div>
            <input type="file" id="cameraInput" accept="image/*" capture="environment" onchange="handlePhotoCapture(event)">
        </div>
        
        <!-- Location -->
        <div class="capture-section">
            <h2 class="section-title">
                <i class="fa fa-map-marker-alt"></i> Location
            </h2>
            <div class="location-section">
                <button type="button" class="location-button" onclick="captureLocation()">
                    <i class="fa fa-crosshairs"></i>
                    Get Current Location
                </button>
                <div class="location-status" id="locationStatus">
                    Location not captured
                </div>
            </div>
            <input type="hidden" id="latitude" name="latitude">
            <input type="hidden" id="longitude" name="longitude">
            
            <div class="form-group">
                <label class="form-label">Street Address</label>
                <input type="text" class="form-control" id="addressStreet" name="address_street" placeholder="123 Main Street">
            </div>
            
            <div class="form-row">
                <div class="form-group">
                    <label class="form-label">City</label>
                    <input type="text" class="form-control" id="addressCity" name="address_city" placeholder="Springfield">
                </div>
                <div class="form-group">
                    <label class="form-label">State</label>
                    <input type="text" class="form-control" id="addressState" name="address_state" placeholder="IL" maxlength="2">
                </div>
            </div>
            
            <div class="form-group">
                <label class="form-label">Postal Code</label>
                <input type="text" class="form-control" id="addressPostal" name="address_postalcode" placeholder="62701">
            </div>
        </div>
        
        <!-- Property Details -->
        <div class="capture-section">
            <h2 class="section-title">
                <i class="fa fa-home"></i> Property Details
            </h2>
            
            <div class="form-group">
                <label class="form-label">Property Name/Description</label>
                <input type="text" class="form-control" id="propertyName" name="name" placeholder="Beautiful 3-bedroom home">
            </div>
            
            <div class="form-group">
                <label class="form-label">Property Type</label>
                <select class="form-control" id="propertyType" name="property_type">
                    <option value="residential">Residential</option>
                    <option value="commercial">Commercial</option>
                    <option value="land">Land</option>
                    <option value="condo">Condo</option>
                    <option value="townhouse">Townhouse</option>
                </select>
            </div>
            
            <div class="form-group">
                <label class="form-label">Listing Price</label>
                <input type="number" class="form-control" id="listingPrice" name="listing_price" placeholder="250000">
            </div>
            
            <div class="form-row">
                <div class="form-group">
                    <label class="form-label">Bedrooms</label>
                    <input type="number" class="form-control" id="bedrooms" name="bedrooms" placeholder="3" min="0">
                </div>
                <div class="form-group">
                    <label class="form-label">Bathrooms</label>
                    <input type="number" class="form-control" id="bathrooms" name="bathrooms" placeholder="2.5" step="0.5" min="0">
                </div>
            </div>
            
            <div class="form-group">
                <label class="form-label">Square Feet</label>
                <input type="number" class="form-control" id="squareFeet" name="square_feet" placeholder="1500">
            </div>
        </div>
        
        <!-- Voice Note -->
        <div class="capture-section">
            <h2 class="section-title">
                <i class="fa fa-microphone"></i> Voice Note
            </h2>
            <div class="voice-note-section">
                <button type="button" class="voice-button" id="voiceButton" onclick="toggleVoiceRecording()">
                    <i class="fa fa-microphone"></i>
                </button>
                <div class="voice-status" id="voiceStatus">
                    Tap to record voice note
                </div>
            </div>
            <input type="hidden" id="voiceNote" name="voice_note">
        </div>
        
        <!-- Submit Buttons -->
        <div class="submit-section">
            <button type="button" class="btn btn-secondary" onclick="saveDraft()">
                <i class="fa fa-save"></i> Save Draft
            </button>
            <button type="submit" class="btn btn-primary">
                <i class="fa fa-check"></i> Save Property
            </button>
        </div>
    </form>
    
    <!-- Loading Overlay -->
    <div class="loading-overlay" id="loadingOverlay">
        <div class="loading-content">
            <div class="spinner"></div>
            <div>Saving property...</div>
        </div>
    </div>
    
    <!-- FontAwesome Icons -->
    <link rel="stylesheet" href="https://cdnjs.cloudflare.com/ajax/libs/font-awesome/5.15.4/css/all.min.css">
    
    <script>
        let capturedImageData = null;
        let voiceRecorder = null;
        let recordedChunks = [];
        let isRecording = false;
        
        // Photo capture
        function handlePhotoCapture(event) {
            const file = event.target.files[0];
            if (file) {
                const reader = new FileReader();
                reader.onload = function(e) {
                    capturedImageData = e.target.result;
                    document.getElementById('photoPreview').src = capturedImageData;
                    document.getElementById('photoPreview').style.display = 'block';
                    document.getElementById('photoPlaceholder').style.display = 'none';
                };
                reader.readAsDataURL(file);
            }
        }
        
        // Location capture
        function captureLocation() {
            const locationButton = event.target.closest('.location-button');
            const locationStatus = document.getElementById('locationStatus');
            
            locationButton.disabled = true;
            locationStatus.textContent = 'Getting location...';
            
            if (navigator.geolocation) {
                navigator.geolocation.getCurrentPosition(
                    function(position) {
                        const lat = position.coords.latitude;
                        const lng = position.coords.longitude;
                        
                        document.getElementById('latitude').value = lat;
                        document.getElementById('longitude').value = lng;
                        
                        locationStatus.textContent = `Location captured: ${lat.toFixed(6)}, ${lng.toFixed(6)}`;
                        locationStatus.style.color = '#28a745';
                        
                        // Mock reverse geocoding
                        setTimeout(() => {
                            if (!document.getElementById('addressStreet').value) {
                                document.getElementById('addressStreet').value = '123 Main Street';
                                document.getElementById('addressCity').value = 'Springfield';
                                document.getElementById('addressState').value = 'IL';
                                document.getElementById('addressPostal').value = '62701';
                            }
                        }, 500);
                        
                        locationButton.disabled = false;
                    },
                    function(error) {
                        locationStatus.textContent = 'Failed to get location';
                        locationStatus.style.color = '#dc3545';
                        locationButton.disabled = false;
                    }
                );
            } else {
                locationStatus.textContent = 'Geolocation not supported';
                locationStatus.style.color = '#dc3545';
                locationButton.disabled = false;
            }
        }
        
        // Voice recording
        async function toggleVoiceRecording() {
            const voiceButton = document.getElementById('voiceButton');
            const voiceStatus = document.getElementById('voiceStatus');
            
            if (!isRecording) {
                try {
                    const stream = await navigator.mediaDevices.getUserMedia({ audio: true });
                    voiceRecorder = new MediaRecorder(stream);
                    recordedChunks = [];
                    
                    voiceRecorder.ondataavailable = event => {
                        recordedChunks.push(event.data);
                    };
                    
                    voiceRecorder.onstop = () => {
                        const blob = new Blob(recordedChunks, { type: 'audio/webm' });
                        const reader = new FileReader();
                        reader.onloadend = () => {
                            document.getElementById('voiceNote').value = reader.result;
                            voiceStatus.textContent = 'Voice note recorded';
                            voiceStatus.style.color = '#28a745';
                        };
                        reader.readAsDataURL(blob);
                        
                        stream.getTracks().forEach(track => track.stop());
                    };
                    
                    voiceRecorder.start();
                    isRecording = true;
                    voiceButton.classList.add('recording');
                    voiceStatus.textContent = 'Recording...';
                    voiceStatus.style.color = '#dc3545';
                } catch (err) {
                    voiceStatus.textContent = 'Microphone access denied';
                    voiceStatus.style.color = '#dc3545';
                }
            } else {
                voiceRecorder.stop();
                isRecording = false;
                voiceButton.classList.remove('recording');
            }
        }
        
        // Form submission
        document.getElementById('quickCaptureForm').addEventListener('submit', async function(e) {
            e.preventDefault();
            
            const loadingOverlay = document.getElementById('loadingOverlay');
            loadingOverlay.style.display = 'flex';
            
            const formData = {
                name: document.getElementById('propertyName').value,
                property_type: document.getElementById('propertyType').value,
                listing_price: document.getElementById('listingPrice').value,
                bedrooms: document.getElementById('bedrooms').value,
                bathrooms: document.getElementById('bathrooms').value,
                square_feet: document.getElementById('squareFeet').value,
                address_street: document.getElementById('addressStreet').value,
                address_city: document.getElementById('addressCity').value,
                address_state: document.getElementById('addressState').value,
                address_postalcode: document.getElementById('addressPostal').value,
                gps_latitude: document.getElementById('latitude').value,
                gps_longitude: document.getElementById('longitude').value,
                image_data: capturedImageData,
                voice_note: document.getElementById('voiceNote').value
            };
            
            try {
                const response = await fetch('index.php?module=RE_Properties&action=QuickCapture', {
                    method: 'POST',
                    headers: {
                        'Content-Type': 'application/json',
                    },
                    body: JSON.stringify(formData)
                });
                
                const result = await response.json();
                
                loadingOverlay.style.display = 'none';
                
                if (result.success) {
                    document.getElementById('successMessage').textContent = 'Property captured successfully!';
                    document.getElementById('successMessage').style.display = 'block';
                    
                    // Redirect after 2 seconds
                    setTimeout(() => {
                        window.location.href = result.mobile_url || 'index.php?module=RE_Properties&action=Mobile';
                    }, 2000);
                } else {
                    document.getElementById('errorMessage').textContent = result.error || 'Failed to save property';
                    document.getElementById('errorMessage').style.display = 'block';
                }
            } catch (error) {
                loadingOverlay.style.display = 'none';
                document.getElementById('errorMessage').textContent = 'Network error: ' + error.message;
                document.getElementById('errorMessage').style.display = 'block';
            }
        });
        
        // Save draft
        function saveDraft() {
            // In a real implementation, this would save to localStorage or IndexedDB
            localStorage.setItem('propertyDraft', JSON.stringify({
                name: document.getElementById('propertyName').value,
                property_type: document.getElementById('propertyType').value,
                listing_price: document.getElementById('listingPrice').value,
                bedrooms: document.getElementById('bedrooms').value,
                bathrooms: document.getElementById('bathrooms').value,
                square_feet: document.getElementById('squareFeet').value,
                address_street: document.getElementById('addressStreet').value,
                address_city: document.getElementById('addressCity').value,
                address_state: document.getElementById('addressState').value,
                address_postalcode: document.getElementById('addressPostal').value,
                latitude: document.getElementById('latitude').value,
                longitude: document.getElementById('longitude').value,
                image_data: capturedImageData,
                voice_note: document.getElementById('voiceNote').value,
                saved_at: new Date().toISOString()
            }));
            
            document.getElementById('successMessage').textContent = 'Draft saved locally';
            document.getElementById('successMessage').style.display = 'block';
            
            setTimeout(() => {
                document.getElementById('successMessage').style.display = 'none';
            }, 3000);
        }
        
        // Load draft on page load
        window.addEventListener('load', function() {
            const draft = localStorage.getItem('propertyDraft');
            if (draft) {
                const draftData = JSON.parse(draft);
                // Populate form with draft data
                // (Implementation omitted for brevity)
            }
        });
        
        // Go back
        function goBack() {
            window.history.back();
        }
    </script>
</body>
</html>
        <?php
    }
}