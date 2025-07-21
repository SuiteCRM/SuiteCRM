-- Payment Bridge Module Tables for SuiteCRM
-- Designed for insurance and financial services integration with COBOL systems

CREATE TABLE IF NOT EXISTS payment_bridge (
    id CHAR(36) NOT NULL PRIMARY KEY,
    name VARCHAR(255),
    date_entered DATETIME,
    date_modified DATETIME,
    modified_user_id CHAR(36),
    created_by CHAR(36),
    description TEXT,
    deleted TINYINT(1) DEFAULT 0,
    assigned_user_id CHAR(36),
    
    -- Payment specific fields
    card_number_masked VARCHAR(20),
    validation_status VARCHAR(20),
    credit_limit DECIMAL(26,6),
    current_balance DECIMAL(26,6),
    apr DECIMAL(5,2),
    last_statement_date DATE,
    last_validation_date DATETIME,
    payment_gateway_status VARCHAR(20),
    cobol_system_health TEXT,
    interest_charge DECIMAL(26,6),
    minimum_payment DECIMAL(26,6),
    payment_plan_status VARCHAR(20),
    
    -- Relationship fields
    account_id CHAR(36),
    
    -- Indices
    INDEX idx_payment_bridge_account (account_id),
    INDEX idx_payment_bridge_card (card_number_masked),
    INDEX idx_payment_bridge_status (validation_status),
    INDEX idx_payment_bridge_deleted (deleted)
) ENGINE=InnoDB DEFAULT CHARSET=utf8;

-- Payment Plans table for managing installment plans
CREATE TABLE IF NOT EXISTS payment_plans (
    id CHAR(36) NOT NULL PRIMARY KEY,
    name VARCHAR(255),
    date_entered DATETIME,
    date_modified DATETIME,
    modified_user_id CHAR(36),
    created_by CHAR(36),
    description TEXT,
    deleted TINYINT(1) DEFAULT 0,
    
    -- Plan details
    payment_bridge_id CHAR(36),
    account_id CHAR(36),
    total_amount DECIMAL(26,6),
    installment_count INT,
    monthly_payment DECIMAL(26,6),
    interest_rate DECIMAL(5,2),
    start_date DATE,
    end_date DATE,
    status VARCHAR(20),
    payments_made INT DEFAULT 0,
    remaining_balance DECIMAL(26,6),
    
    -- Indices
    INDEX idx_payment_plans_bridge (payment_bridge_id),
    INDEX idx_payment_plans_account (account_id),
    INDEX idx_payment_plans_status (status),
    INDEX idx_payment_plans_deleted (deleted)
) ENGINE=InnoDB DEFAULT CHARSET=utf8;

-- Payment History table for tracking all transactions
CREATE TABLE IF NOT EXISTS payment_history (
    id CHAR(36) NOT NULL PRIMARY KEY,
    date_entered DATETIME,
    date_modified DATETIME,
    
    -- Transaction details
    payment_bridge_id CHAR(36),
    account_id CHAR(36),
    transaction_type VARCHAR(50),
    amount DECIMAL(26,6),
    balance_before DECIMAL(26,6),
    balance_after DECIMAL(26,6),
    reference_number VARCHAR(50),
    status VARCHAR(20),
    cobol_response TEXT,
    
    -- Indices
    INDEX idx_payment_history_bridge (payment_bridge_id),
    INDEX idx_payment_history_account (account_id),
    INDEX idx_payment_history_date (date_entered),
    INDEX idx_payment_history_type (transaction_type)
) ENGINE=InnoDB DEFAULT CHARSET=utf8;

-- COBOL System Health Metrics table
CREATE TABLE IF NOT EXISTS cobol_health_metrics (
    id CHAR(36) NOT NULL PRIMARY KEY,
    date_entered DATETIME,
    
    -- Health metrics
    response_time_ms INT,
    status VARCHAR(20),
    availability_percent DECIMAL(5,2),
    failed_transactions INT,
    successful_transactions INT,
    queue_depth INT,
    error_messages TEXT,
    
    -- Indices
    INDEX idx_cobol_health_date (date_entered),
    INDEX idx_cobol_health_status (status)
) ENGINE=InnoDB DEFAULT CHARSET=utf8;

-- Add custom fields to Accounts module for payment integration
ALTER TABLE accounts 
ADD COLUMN IF NOT EXISTS payment_method_c VARCHAR(50),
ADD COLUMN IF NOT EXISTS card_on_file_c TINYINT(1) DEFAULT 0,
ADD COLUMN IF NOT EXISTS credit_limit_c DECIMAL(26,6),
ADD COLUMN IF NOT EXISTS current_balance_c DECIMAL(26,6),
ADD COLUMN IF NOT EXISTS payment_status_c VARCHAR(20),
ADD COLUMN IF NOT EXISTS last_payment_date_c DATE,
ADD COLUMN IF NOT EXISTS payment_bridge_id_c CHAR(36);

-- Create relationship table
CREATE TABLE IF NOT EXISTS accounts_payment_bridge_c (
    id VARCHAR(36) NOT NULL PRIMARY KEY,
    date_modified DATETIME,
    deleted TINYINT(1) DEFAULT 0,
    accounts_ida VARCHAR(36),
    payment_bridge_idb VARCHAR(36),
    INDEX idx_accounts_payment_bridge_ida (accounts_ida),
    INDEX idx_accounts_payment_bridge_idb (payment_bridge_idb),
    INDEX idx_accounts_payment_bridge_deleted (deleted)
) ENGINE=InnoDB DEFAULT CHARSET=utf8;

-- Insert dropdown values
INSERT INTO config (category, name, value) VALUES 
('PaymentBridge', 'payment_validation_status_list', 'Valid^Valid|Invalid^Invalid|Pending^Pending|Error^Error'),
('PaymentBridge', 'payment_gateway_status_list', 'Active^Active|Inactive^Inactive|Error^Error|Maintenance^Maintenance'),
('PaymentBridge', 'payment_plan_status_list', 'Active^Active|Completed^Completed|Defaulted^Defaulted|Cancelled^Cancelled')
ON DUPLICATE KEY UPDATE value = VALUES(value);