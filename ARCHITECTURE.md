# SuiteCRM + COBOL Integration Architecture

## Overview
This architecture bridges SuiteCRM's PHP-based CRM system with COBOL financial processing engines, enabling regional banks to leverage both modern CRM features and mainframe-grade financial calculations.

## Architecture Diagram
```
┌─────────────────┐     ┌──────────────────┐     ┌─────────────────┐
│   SuiteCRM UI   │────▶│  PHP Business    │────▶│ COBOL Bridge    │
│   (Browser)     │     │     Logic        │     │    Module       │
└─────────────────┘     └──────────────────┘     └────────┬────────┘
                                                           │
                        ┌──────────────────┐               ▼
                        │   API Gateway    │◀──────────────┤
                        │   (Node.js)      │               
                        └────────┬─────────┘               
                                 │                         
                    ┌────────────┴────────────┐            
                    ▼                         ▼            
        ┌──────────────────┐       ┌──────────────────┐   
        │ COBOL Calc Engine│       │ Mainframe Sync   │   
        │   (REST API)     │       │    Service       │   
        └──────────────────┘       └──────────────────┘   
                    │                         │            
                    └─────────┬───────────────┘            
                              ▼                            
                    ┌──────────────────┐                   
                    │   Redis Queue    │                   
                    │ (Async Process)  │                   
                    └──────────────────┘                   
```

## Component Details

### 1. COBOL Bridge Module (Custom SuiteCRM Module)
- Location: `/custom/modules/COBOL_Bridge/`
- Extends SugarBean for database operations
- Provides PHP interfaces to COBOL services
- Handles response caching and error management

### 2. API Gateway (Node.js)
- Routes requests between SuiteCRM and COBOL services
- Implements circuit breaker pattern for resilience
- Provides request/response transformation
- Handles authentication token management

### 3. COBOL Calculation Engine
- Financial calculation microservice
- Implements banking-specific algorithms:
  - Interest calculations (simple, compound, amortization)
  - Loan payment schedules
  - Currency conversion with exact precision
  - Tax calculations

### 4. Integration Points

#### A. AOS_Quotes Module Enhancement
- Replace `calculateLineItemTotals()` with COBOL calls
- Add precision validation for currency fields
- Implement audit trail for calculations

#### B. AOS_Invoices Module Enhancement  
- COBOL-based tax calculation
- Batch invoice generation via COBOL
- Statement generation with exact balances

#### C. Workflow Integration
- New workflow action: "Execute COBOL Program"
- Conditional logic based on COBOL results
- Scheduled COBOL batch jobs

### 5. Data Flow

#### Synchronous Flow (Real-time Calculations)
1. User enters quote line items in SuiteCRM
2. JavaScript triggers calculation on blur
3. PHP controller calls COBOL Bridge
4. API Gateway routes to COBOL engine
5. COBOL performs calculation
6. Result returned and displayed immediately

#### Asynchronous Flow (Batch Processing)
1. Scheduled job triggers batch process
2. PHP queues job in Redis
3. Worker polls Redis queue
4. COBOL processes batch file
5. Results written to database
6. Users notified of completion

### 6. Security Architecture
- API Gateway implements OAuth 2.0
- COBOL services run in isolated containers
- End-to-end encryption for financial data
- Audit logging for all calculations
- Role-based access control integration

### 7. Performance Optimization
- Redis caching for repeated calculations
- Connection pooling for COBOL services
- Horizontal scaling of COBOL containers
- Database query optimization
- CDN for static assets

### 8. Error Handling
- Graceful degradation to PHP calculations
- Comprehensive error logging
- User-friendly error messages
- Automatic retry with exponential backoff
- Dead letter queue for failed jobs

## Implementation Priority
1. **Phase 1**: COBOL Calculation Engine (Week 1)
2. **Phase 2**: Quote/Invoice Integration (Week 2)
3. **Phase 3**: Batch Processing (Week 3)
4. **Phase 4**: Mainframe Sync (Week 4)
5. **Phase 5**: Full Workflow Integration (Week 5)
6. **Phase 6**: Performance & Security (Week 6)