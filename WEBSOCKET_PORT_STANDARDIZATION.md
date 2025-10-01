# WebSocket Port Standardization Summary

## Overview
This document summarizes the WebSocket port configuration standardization performed across the SuiteCRM COBOL Banking Integration codebase.

## Changes Made

### 1. Port Standardization
- **Standardized WebSocket Port**: 8081 (previously mixed between 8080 and 8081)
- **REST API Port**: 8080 (remains unchanged)
- **Monitoring Port**: 8090 (remains unchanged)

### 2. Configuration Files Created

#### `/config/cobol_websocket.php`
- Central PHP configuration file for WebSocket settings
- Provides helper functions for getting correct URLs based on environment
- Supports both Docker and local development environments
- Key settings:
  - `websocket_port`: 8081
  - `websocket_url`: ws://localhost:8081
  - `websocket_url_docker`: ws://cobol-gateway:8081

#### `/.env.example`
- Environment configuration template
- Defines all port settings as environment variables
- Allows easy configuration changes without code modifications

### 3. Code Updates

#### WebSocket Server (`/cobol-services/websocket-server.js`)
- Updated to use port 8081 by default
- Now reads from environment variables (COBOL_WS_PORT or WEBSOCKET_PORT)
- Falls back to 8081 if no environment variable is set

#### COBOL Payment Service (`/modules/COBOLIntegration/services/COBOLPaymentService.php`)
- Updated to use the central configuration file
- Now reads WebSocket URL from `$sugar_config['cobol_websocket_url']`
- Maintains backward compatibility

#### Shell Scripts (`/cobol-services/start-servers.sh`)
- Updated echo message to reflect correct port 8081

### 4. Documentation Updates

#### `/COBOL-INTEGRATION.md`
- Updated WebSocket endpoint from ws://localhost:8080 to ws://localhost:8081
- Updated configuration example to use port 8081

### 5. Verified Files (Already Correct)
The following files were checked and already had the correct port configuration:
- `/docker-compose.yml` - Correct port mappings
- `/apache-config.conf` - Correct WebSocket proxy configuration
- `/suitecrm_banking_demo.html` - Using port 8081
- `/test_cobol_integration.php` - Testing port 8081
- `/demo-scripts/6-transaction-dashboard.md` - References port 8081
- `/COBOL_INTEGRATION_GUIDE.md` - Using port 8081

## Configuration Priority

The system now uses the following priority for WebSocket configuration:

1. Environment variables (COBOL_WS_PORT or WEBSOCKET_PORT)
2. SuiteCRM configuration (`$sugar_config['cobol_websocket_url']`)
3. Central configuration file (`/config/cobol_websocket.php`)
4. Hardcoded default (port 8081)

## Usage

### For Docker Deployments
```bash
docker-compose up -d
```
The docker-compose.yml file already has the correct port mappings.

### For Local Development
1. Copy `.env.example` to `.env`
2. Adjust ports if needed
3. Start services with environment variables loaded

### For SuiteCRM Integration
Include the configuration file in your PHP code:
```php
require_once 'config/cobol_websocket.php';
$websocketUrl = getCobolWebSocketUrl();
```

## Benefits

1. **Consistency**: All components now use the same WebSocket port
2. **Flexibility**: Easy to change ports via environment variables
3. **Docker Support**: Automatic URL switching for containerized environments
4. **Maintainability**: Central configuration reduces hardcoded values
5. **Backward Compatibility**: Existing configurations continue to work

## Testing

To verify the WebSocket connection:
```bash
# Test WebSocket port availability
nc -zv localhost 8081

# Run integration test
php test_cobol_integration.php

# Check WebSocket server logs
docker logs cobol-gateway
```

## Migration Notes

For existing installations:
1. Update any hardcoded references to port 8080 for WebSocket to 8081
2. Include the new configuration file in custom modules
3. Restart WebSocket server after changes
4. Clear SuiteCRM cache after configuration updates