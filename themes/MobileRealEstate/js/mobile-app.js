/**
 * SuiteCRM Mobile Real Estate App
 * Progressive Web App functionality with offline support
 */

// Service Worker Registration
if ('serviceWorker' in navigator) {
    navigator.serviceWorker.register('/themes/MobileRealEstate/js/service-worker.js')
        .then(reg => console.log('Service Worker registered'))
        .catch(err => console.error('Service Worker registration failed:', err));
}

// Global app state
const app = {
    isOnline: navigator.onLine,
    pendingSync: [],
    currentLocation: null,
    properties: [],
    deferredPrompt: null
};

// Initialize mobile app
function initializeMobileApp() {
    // Request location permission
    if ('geolocation' in navigator) {
        navigator.geolocation.getCurrentPosition(
            position => {
                app.currentLocation = {
                    lat: position.coords.latitude,
                    lng: position.coords.longitude
                };
                updateNearbyProperties();
            },
            error => console.error('Location error:', error)
        );
    }
    
    // Setup offline detection
    window.addEventListener('online', () => {
        app.isOnline = true;
        document.getElementById('offlineIndicator').classList.remove('active');
        syncPendingData();
    });
    
    window.addEventListener('offline', () => {
        app.isOnline = false;
        document.getElementById('offlineIndicator').classList.add('active');
    });
    
    // Setup touch gestures
    setupTouchGestures();
    
    // Initialize camera if available
    if ('mediaDevices' in navigator) {
        initializeCamera();
    }
}

// Touch gesture support
function setupTouchGestures() {
    let touchStartX = 0;
    let touchEndX = 0;
    
    document.addEventListener('touchstart', e => {
        touchStartX = e.changedTouches[0].screenX;
    });
    
    document.addEventListener('touchend', e => {
        touchEndX = e.changedTouches[0].screenX;
        handleSwipe();
    });
    
    function handleSwipe() {
        if (touchEndX < touchStartX - 50) {
            // Swipe left - close menu
            document.getElementById('mobileNav').classList.remove('active');
        }
        if (touchEndX > touchStartX + 50) {
            // Swipe right - open menu
            document.getElementById('mobileNav').classList.add('active');
        }
    }
}

// Toggle mobile navigation
function toggleMobileNav() {
    document.getElementById('mobileNav').classList.toggle('active');
}

// Property Quick Capture
function toggleCaptureOptions() {
    document.getElementById('captureOptions').classList.toggle('active');
}

// Capture photo
async function capturePhoto() {
    try {
        const stream = await navigator.mediaDevices.getUserMedia({ 
            video: { facingMode: 'environment' } 
        });
        
        // Create video element
        const video = document.createElement('video');
        video.srcObject = stream;
        video.play();
        
        // Create capture UI
        const captureUI = createCaptureUI(video, stream);
        document.body.appendChild(captureUI);
        
    } catch (error) {
        console.error('Camera error:', error);
        alert('Camera access denied or not available');
    }
}

// Create camera capture UI
function createCaptureUI(video, stream) {
    const overlay = document.createElement('div');
    overlay.className = 'camera-overlay';
    overlay.innerHTML = `
        <div class="camera-container">
            <video id="cameraFeed" autoplay playsinline></video>
            <div class="camera-controls">
                <button onclick="captureImage()">
                    <i class="fa fa-camera"></i>
                </button>
                <button onclick="closeCamera()">
                    <i class="fa fa-times"></i>
                </button>
            </div>
        </div>
    `;
    
    overlay.querySelector('#cameraFeed').srcObject = stream;
    
    window.captureImage = () => {
        const canvas = document.createElement('canvas');
        canvas.width = video.videoWidth;
        canvas.height = video.videoHeight;
        canvas.getContext('2d').drawImage(video, 0, 0);
        
        const imageData = canvas.toDataURL('image/jpeg');
        processPropertyImage(imageData);
        
        closeCamera();
    };
    
    window.closeCamera = () => {
        stream.getTracks().forEach(track => track.stop());
        overlay.remove();
    };
    
    return overlay;
}

// Process captured property image
function processPropertyImage(imageData) {
    const propertyData = {
        image_data: imageData,
        gps_latitude: app.currentLocation?.lat,
        gps_longitude: app.currentLocation?.lng,
        capture_time: new Date().toISOString()
    };
    
    if (app.isOnline) {
        // Send to server
        $.ajax({
            url: 'index.php?module=RE_Properties&action=QuickCapture',
            method: 'POST',
            data: propertyData,
            success: response => {
                showNotification('Property captured successfully!');
                if (response.id) {
                    navigateToProperty(response.id);
                }
            },
            error: () => {
                // Save for offline sync
                saveOfflineData('property_capture', propertyData);
            }
        });
    } else {
        // Save for offline sync
        saveOfflineData('property_capture', propertyData);
        showNotification('Property saved offline. Will sync when connected.');
    }
}

// Voice note recording
function recordVoiceNote() {
    if (!('MediaRecorder' in window)) {
        alert('Voice recording not supported on this device');
        return;
    }
    
    navigator.mediaDevices.getUserMedia({ audio: true })
        .then(stream => {
            const mediaRecorder = new MediaRecorder(stream);
            const audioChunks = [];
            
            // Create recording UI
            const recordingUI = createRecordingUI(mediaRecorder, audioChunks, stream);
            document.body.appendChild(recordingUI);
            
            mediaRecorder.start();
            
            mediaRecorder.addEventListener('dataavailable', event => {
                audioChunks.push(event.data);
            });
            
            mediaRecorder.addEventListener('stop', () => {
                const audioBlob = new Blob(audioChunks, { type: 'audio/wav' });
                processVoiceNote(audioBlob);
            });
        })
        .catch(error => {
            console.error('Microphone error:', error);
            alert('Microphone access denied');
        });
}

// Create voice recording UI
function createRecordingUI(mediaRecorder, audioChunks, stream) {
    const overlay = document.createElement('div');
    overlay.className = 'recording-overlay';
    overlay.innerHTML = `
        <div class="recording-container">
            <div class="recording-indicator">
                <i class="fa fa-microphone"></i>
                <span>Recording...</span>
            </div>
            <div class="recording-timer">00:00</div>
            <button class="stop-recording" onclick="stopRecording()">
                <i class="fa fa-stop"></i> Stop Recording
            </button>
        </div>
    `;
    
    let startTime = Date.now();
    const timerInterval = setInterval(() => {
        const elapsed = Math.floor((Date.now() - startTime) / 1000);
        const minutes = Math.floor(elapsed / 60).toString().padStart(2, '0');
        const seconds = (elapsed % 60).toString().padStart(2, '0');
        overlay.querySelector('.recording-timer').textContent = `${minutes}:${seconds}`;
    }, 1000);
    
    window.stopRecording = () => {
        mediaRecorder.stop();
        stream.getTracks().forEach(track => track.stop());
        clearInterval(timerInterval);
        overlay.remove();
    };
    
    return overlay;
}

// Location capture
function captureLocation() {
    if (!('geolocation' in navigator)) {
        alert('Location services not available');
        return;
    }
    
    navigator.geolocation.getCurrentPosition(
        position => {
            const locationData = {
                latitude: position.coords.latitude,
                longitude: position.coords.longitude,
                accuracy: position.coords.accuracy,
                timestamp: new Date().toISOString()
            };
            
            // Reverse geocode
            reverseGeocode(locationData.latitude, locationData.longitude)
                .then(address => {
                    locationData.address = address;
                    createPropertyFromLocation(locationData);
                });
        },
        error => {
            alert('Could not get location: ' + error.message);
        },
        {
            enableHighAccuracy: true,
            timeout: 10000,
            maximumAge: 0
        }
    );
}

// QR Code Scanner
function scanQRCode() {
    // Create QR scanner UI
    const scannerUI = document.createElement('div');
    scannerUI.className = 'qr-scanner-overlay';
    scannerUI.innerHTML = `
        <div class="qr-scanner-container">
            <video id="qrVideo" autoplay playsinline></video>
            <div class="qr-scanner-frame"></div>
            <button class="close-scanner" onclick="closeQRScanner()">
                <i class="fa fa-times"></i>
            </button>
        </div>
    `;
    
    document.body.appendChild(scannerUI);
    
    // Initialize QR scanner
    const video = document.getElementById('qrVideo');
    
    navigator.mediaDevices.getUserMedia({ 
        video: { facingMode: 'environment' } 
    }).then(stream => {
        video.srcObject = stream;
        
        // Use QR scanning library or API
        const scanner = new QRScanner(video, result => {
            handleQRCode(result);
            closeQRScanner();
        });
        
        scanner.start();
        
        window.closeQRScanner = () => {
            scanner.stop();
            stream.getTracks().forEach(track => track.stop());
            scannerUI.remove();
        };
    });
}

// Load properties with mobile optimization
function loadProperties() {
    const params = {
        mobile: 1,
        limit: 20
    };
    
    if (app.currentLocation) {
        params.user_lat = app.currentLocation.lat;
        params.user_lng = app.currentLocation.lng;
    }
    
    $.ajax({
        url: 'index.php?module=RE_Properties&action=MobileList',
        data: params,
        success: response => {
            app.properties = response.properties;
            renderProperties();
        },
        error: () => {
            // Load from cache if offline
            loadOfflineProperties();
        }
    });
}

// Render property cards
function renderProperties() {
    const grid = document.getElementById('propertyGrid');
    grid.innerHTML = '';
    
    app.properties.forEach(property => {
        const card = createPropertyCard(property);
        grid.appendChild(card);
    });
}

// Create property card element
function createPropertyCard(property) {
    const card = document.createElement('div');
    card.className = 'property-card touchable';
    card.innerHTML = `
        <img src="${property.image || 'themes/MobileRealEstate/images/property-placeholder.jpg'}" 
             alt="${property.name}" class="property-image">
        ${property.status === 'new' ? '<div class="property-badge">New</div>' : ''}
        <div class="property-details">
            <div class="property-price">$${formatNumber(property.listing_price)}</div>
            <div class="property-address">${property.address_city}</div>
            <div class="property-features">
                <div class="feature">
                    <i class="fa fa-bed"></i>
                    <span>${property.bedrooms}</span>
                </div>
                <div class="feature">
                    <i class="fa fa-bath"></i>
                    <span>${property.bathrooms}</span>
                </div>
                <div class="feature">
                    <i class="fa fa-ruler-combined"></i>
                    <span>${formatNumber(property.square_feet)}</span>
                </div>
            </div>
        </div>
        <div class="property-actions">
            <div class="action" onclick="viewProperty('${property.id}')">
                <i class="fa fa-eye"></i>
                <span>View</span>
            </div>
            <div class="action" onclick="shareProperty('${property.id}')">
                <i class="fa fa-share"></i>
                <span>Share</span>
            </div>
            <div class="action" onclick="scheduleShowing('${property.id}')">
                <i class="fa fa-calendar"></i>
                <span>Schedule</span>
            </div>
        </div>
    `;
    
    return card;
}

// PWA Installation
function setupPWA() {
    // Capture install prompt
    window.addEventListener('beforeinstallprompt', e => {
        e.preventDefault();
        app.deferredPrompt = e;
        
        // Show install prompt after 30 seconds
        setTimeout(() => {
            if (app.deferredPrompt) {
                document.getElementById('pwaPrompt').classList.add('active');
            }
        }, 30000);
    });
    
    // Check if already installed
    if (window.matchMedia('(display-mode: standalone)').matches) {
        console.log('PWA is already installed');
    }
}

// Install PWA
function installPWA() {
    if (app.deferredPrompt) {
        app.deferredPrompt.prompt();
        app.deferredPrompt.userChoice.then(choiceResult => {
            if (choiceResult.outcome === 'accepted') {
                console.log('PWA installed');
            }
            app.deferredPrompt = null;
            document.getElementById('pwaPrompt').classList.remove('active');
        });
    }
}

// Offline data management
function saveOfflineData(type, data) {
    const offlineData = JSON.parse(localStorage.getItem('offlineData') || '[]');
    offlineData.push({
        id: Date.now(),
        type: type,
        data: data,
        timestamp: new Date().toISOString()
    });
    localStorage.setItem('offlineData', JSON.stringify(offlineData));
}

// Sync offline data when online
function syncPendingData() {
    const offlineData = JSON.parse(localStorage.getItem('offlineData') || '[]');
    
    offlineData.forEach(item => {
        $.ajax({
            url: 'index.php?module=RE_Properties&action=SyncOffline',
            method: 'POST',
            data: item,
            success: () => {
                // Remove synced item
                const remaining = offlineData.filter(d => d.id !== item.id);
                localStorage.setItem('offlineData', JSON.stringify(remaining));
            }
        });
    });
}

// Utility functions
function formatNumber(num) {
    return num.toString().replace(/\B(?=(\d{3})+(?!\d))/g, ",");
}

function showNotification(message) {
    const notification = document.createElement('div');
    notification.className = 'mobile-notification';
    notification.textContent = message;
    document.body.appendChild(notification);
    
    setTimeout(() => {
        notification.classList.add('show');
    }, 100);
    
    setTimeout(() => {
        notification.classList.remove('show');
        setTimeout(() => notification.remove(), 300);
    }, 3000);
}

// Navigation functions
function navigateTo(module) {
    window.location.href = `index.php?module=${module}&action=Mobile`;
}

function viewProperty(id) {
    window.location.href = `index.php?module=RE_Properties&action=Mobile&record=${id}`;
}

function shareProperty(id) {
    if (navigator.share) {
        navigator.share({
            title: 'Check out this property',
            url: `${window.location.origin}/index.php?module=RE_Properties&action=Mobile&record=${id}`
        });
    } else {
        // Fallback
        const url = `${window.location.origin}/index.php?module=RE_Properties&action=Mobile&record=${id}`;
        navigator.clipboard.writeText(url);
        showNotification('Link copied to clipboard');
    }
}

// COBOL Integration Functions
function updatePaymentStatus(data) {
    const statusEl = document.getElementById('validationStatus');
    if (statusEl) {
        if (data.status === 'processing') {
            statusEl.innerHTML = '<div class="spinner"></div> Processing with COBOL system...';
        } else if (data.status === 'approved') {
            statusEl.className = 'validation-status valid';
            statusEl.innerHTML = `<i class="fa fa-check-circle"></i> Approved - Auth: ${data.auth_code}`;
        } else {
            statusEl.className = 'validation-status invalid';
            statusEl.innerHTML = `<i class="fa fa-times-circle"></i> ${data.message}`;
        }
    }
}

function showValidationResult(data) {
    handlePaymentResponse(data);
}