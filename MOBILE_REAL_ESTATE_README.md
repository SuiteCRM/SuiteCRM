# SuiteCRM Mobile Real Estate Edition

A revolutionary mobile-first CRM for real estate professionals, featuring native COBOL integration for payment processing and legacy system connectivity.

## 🚀 Key Features

### 1. **Property Quick Capture**
- 📸 Instant photo capture with GPS tagging
- 🎤 Voice-to-text property notes
- 📍 Automatic address detection via GPS
- 🎥 360° virtual tour creation
- 📱 QR code generation for instant sharing

### 2. **COBOL-Powered Financial Processing**
- 💳 Real-time credit card validation
- 🏦 Native COBOL integration for banking systems
- 💰 Instant earnest money processing
- 📊 Commission calculations
- 🔒 PCI-compliant payment handling

### 3. **Mobile-First Design**
- 📱 Progressive Web App (PWA) with offline support
- 🌐 Touch-optimized interface
- 📶 Smart sync when reconnected
- 🔔 Push notifications for new listings
- 📍 Location-based property alerts

### 4. **Real Estate Specific Tools**
- 🏠 Property comparison tool
- 📅 Showing scheduler with route optimization
- 📧 Automated client matching
- 📈 Market analytics dashboard
- 🤝 Digital signature integration

### 5. **Advanced Communication**
- 💬 SMS/MMS integration
- 📹 Video messaging
- 💬 Live chat widget
- 📱 Social media posting
- ✉️ Smart email templates

### 6. **Offline Capabilities**
- 💾 Local data storage
- 🔄 Intelligent sync
- 📱 Full offline functionality
- ⚡ Bandwidth optimization
- 🔐 Encrypted local storage

## 🛠️ Quick Start

### Prerequisites
- Docker & Docker Compose
- Node.js 16+
- Git

### Installation

1. **Clone the repository**
```bash
cd /Users/jfuginay/Documents/dev/SuiteCRM-fork
```

2. **Install dependencies**
```bash
# Install COBOL gateway dependencies
cd cobol/gateway && npm install && cd ../..
```

3. **Start services with Docker**
```bash
docker-compose up -d
```

4. **Access the application**
- Web: http://localhost:8082
- Mobile: http://localhost:8082/index.php?module=RE_Properties&action=Mobile
- COBOL Gateway: http://localhost:8080/health

## 📱 Mobile Access

### Install as PWA
1. Open in mobile browser
2. Click "Add to Home Screen" prompt
3. Launch from home screen for app-like experience

### Enable Location Services
1. Allow location access when prompted
2. Properties will auto-sort by distance
3. Get alerts for nearby listings

## 💳 COBOL Payment Processing

### Test Credit Cards
```
Visa: 4111111111111111
MasterCard: 5555555555554444
Amex: 378282246310005
```

### COBOL Integration Points
- `/cobol/validate-card` - Card validation
- `/cobol/process-payment` - Payment processing
- `/cobol/calculate-mortgage` - Mortgage calculations
- WebSocket on port 8081 for real-time updates

## 🏗️ Architecture

### Services
- **SuiteCRM**: Main application (PHP)
- **MySQL**: Database
- **COBOL Gateway**: Node.js bridge to COBOL
- **COBOL Runtime**: GnuCOBOL programs
- **Redis**: Caching and sessions

### Mobile Stack
- Progressive Web App (PWA)
- Service Workers for offline
- WebSockets for real-time
- Touch-optimized UI
- GPS integration

## 🔧 Development

### Add New COBOL Program
1. Create `.cob` file in `cobol/programs/`
2. Gateway auto-compiles on startup
3. Add endpoint in `cobol-gateway.js`

### Customize Mobile Theme
- Edit `themes/MobileRealEstate/css/mobile-realestate.scss`
- Modify `themes/MobileRealEstate/js/mobile-app.js`
- Update manifest in `themes/MobileRealEstate/manifest.json`

## 📊 API Examples

### Quick Capture Property
```javascript
POST /index.php?module=RE_Properties&action=QuickCapture
{
  "image_data": "base64...",
  "gps_latitude": 37.7749,
  "gps_longitude": -122.4194,
  "voice_note": "audio/wav;base64..."
}
```

### Validate Payment
```javascript
POST /cobol/validate-card
{
  "card_number": "4111111111111111",
  "expiry": "12/25",
  "cvv": "123",
  "amount": 5000.00,
  "property_id": "prop_123"
}
```

### WebSocket Connection
```javascript
const ws = new WebSocket('ws://localhost:8081');
ws.send(JSON.stringify({
  action: 'subscribe',
  property_id: 'prop_123'
}));
```

## 🚀 Deployment

### Production Setup
1. Update `.env` with production values
2. Enable SSL in nginx config
3. Set up domain and SSL certificates
4. Configure backup strategy
5. Enable monitoring

### Performance Optimization
- Enable Redis caching
- Use CDN for static assets
- Enable gzip compression
- Optimize images
- Minify CSS/JS

## 📱 Mobile Features Usage

### Camera Capture
1. Tap the floating + button
2. Select camera icon
3. Take photo
4. Auto-saves with GPS location

### Voice Notes
1. Tap microphone icon
2. Speak property details
3. Auto-transcribes to text
4. Saves to property notes

### QR Code Sharing
1. Open property details
2. Tap "Generate QR"
3. Share or print code
4. Clients scan to view

### Offline Mode
1. Works automatically
2. Yellow bar shows offline status
3. Changes sync when reconnected
4. Full functionality maintained

## 🔒 Security

- PCI DSS compliant payment processing
- SSL/TLS encryption
- Biometric authentication support
- Encrypted local storage
- Audit trails for all transactions

## 📞 Support

For issues or questions:
- GitHub Issues: [Your GitHub URL]
- Documentation: See `/docs` folder
- COBOL Support: See `COBOL_INTEGRATION_GUIDE.md`

## 🎯 Roadmap

- [ ] AI-powered property recommendations
- [ ] Blockchain property records
- [ ] AR property viewing
- [ ] Voice assistant integration
- [ ] Multi-language support

---

Built with ❤️ for real estate professionals who demand mobile excellence and enterprise-grade reliability.