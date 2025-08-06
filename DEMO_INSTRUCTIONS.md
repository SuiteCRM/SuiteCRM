# SuiteCRM + COBOL Banking Integration Demo Instructions

## Overview
This demo showcases a mobile-optimized SuiteCRM real estate application integrated with COBOL banking services. The integration demonstrates how modern web/mobile applications can seamlessly work with legacy mainframe systems.

## Prerequisites
- Docker and Docker Compose installed
- Port availability: 3306 (MySQL), 6379 (Redis), 8080-8081 (COBOL), 8082 (SuiteCRM)
- Web browser with JavaScript enabled

## Starting the Demo

### 1. Build and Start All Services
```bash
# From the SuiteCRM-fork directory
docker-compose build
docker-compose up -d
```

### 2. Verify Services are Running
```bash
docker-compose ps
```

All services should show as "Up":
- suitecrm-mysql
- suitecrm-mobile
- cobol-gateway
- cobol-runtime
- suitecrm-redis

### 3. Initialize SuiteCRM (First Time Only)
1. Open http://localhost:8082 in your browser
2. If this is the first run, you'll see the SuiteCRM installer
3. Use these database settings:
   - Host: mysql
   - Database: suitecrm
   - User: suitecrm
   - Password: suitecrm_pass
4. Complete the installation wizard

## Demo Scenarios

### Scenario 1: COBOL Banking Services (Standalone)
**URL:** http://localhost:8082/banking_demo.html

This demonstrates direct COBOL integration:
1. **Card Validator**: Enter test card number 4111111111111111
2. **Loan Calculator**: Calculate mortgage payments with COBOL
3. **Account Verify**: Verify bank routing/account numbers

### Scenario 2: Mobile Real Estate App
**URL:** http://localhost:8082/index.php?module=RE_Properties&action=Mobile

Features to demonstrate:
1. **PWA Capabilities**: Add to home screen prompt
2. **Property Browsing**: Swipe through listings
3. **Payment Processing**: Process earnest money via COBOL
4. **Offline Mode**: Works without internet (sync when reconnected)

### Scenario 3: Full Integration Demo
**URL:** http://localhost:8082/suitecrm_banking_demo.html

This shows the complete integration:
1. **Property Payment Processing**
   - Property ID: PROP-2024-001
   - Test Card: 4111111111111111
   - Shows real-time COBOL validation

2. **Mortgage Pre-Qualification**
   - Instant qualification check via COBOL
   - Shows monthly payment calculations

3. **Account Verification**
   - Routing: 123456789
   - Account: 1234567890
   - Verifies buyer has sufficient funds

### Scenario 4: SuiteCRM Dashboard with COBOL Widget
**URL:** http://localhost:8082/cobol_banking_direct.php

Shows COBOL services integrated into SuiteCRM dashboard.

## Mobile Testing

### Testing on Mobile Device
1. Ensure your mobile device is on the same network
2. Find your computer's IP address:
   ```bash
   # macOS
   ipconfig getifaddr en0
   ```
3. Access via mobile browser: http://[YOUR-IP]:8082/index.php?module=RE_Properties&action=Mobile

### Mobile Features to Test
- Camera integration for property photos
- GPS location for nearby properties
- Voice notes for property descriptions
- QR code scanning for property info
- Offline functionality

## WebSocket Real-Time Features
The COBOL gateway supports WebSocket connections for real-time updates:
1. Open developer console in browser
2. Watch for WebSocket messages during payment processing
3. See real-time transaction status updates

## Troubleshooting

### If COBOL services show offline:
```bash
docker logs cobol-gateway
docker restart cobol-gateway
```

### If SuiteCRM won't load:
```bash
docker logs suitecrm-mobile
# Check permissions
docker exec suitecrm-mobile chown -R www-data:www-data /var/www/html
```

### If Redis connection fails:
```bash
docker logs suitecrm-redis
docker restart suitecrm-redis
```

## Demo Talk Track

### Opening
"Today I'll demonstrate how SuiteCRM can integrate with legacy COBOL banking systems to create a modern mobile real estate application."

### Key Points
1. **Legacy Integration**: "Notice how seamlessly the 40-year-old COBOL programs validate credit cards in real-time"
2. **Mobile First**: "The interface is fully responsive and works as a Progressive Web App"
3. **Offline Capable**: "Agents can work in buildings with poor connectivity"
4. **Real-Time Updates**: "WebSocket connections provide instant transaction feedback"

### Closing
"This demonstrates how organizations can modernize their user experience while preserving valuable legacy business logic."

## Advanced Features

### Monitoring
- COBOL Gateway health: http://localhost:8080/health
- WebSocket status: http://localhost:8080/ws-status
- Redis queue info: http://localhost:8080/api/redis-status

### Performance Testing
Use the included test endpoints:
- Load test: http://localhost:8080/api/load-test
- Stress test multiple COBOL calls simultaneously

## Stopping the Demo
```bash
docker-compose down
# To also remove volumes (database data):
docker-compose down -v
```

## Additional Resources
- COBOL programs source: `/cobol/programs/`
- Mobile theme: `/themes/MobileRealEstate/`
- Integration module: `/modules/COBOLIntegration/`

For questions or issues, check the logs:
```bash
docker-compose logs -f [service-name]
```