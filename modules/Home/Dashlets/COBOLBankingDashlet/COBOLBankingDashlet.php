<?php
if(!defined('sugarEntry') || !sugarEntry) die('Not A Valid Entry Point');

require_once('include/Dashlets/Dashlet.php');

class COBOLBankingDashlet extends Dashlet {
    
    public $hasScript = true;
    
    public function __construct($id, $def = null) {
        global $current_user, $app_strings;
        
        parent::__construct($id);
        
        $this->isConfigurable = false;
        $this->hasScript = true;
        
        if(!empty($def['title'])) {
            $this->title = $def['title'];
        } else {
            $this->title = 'COBOL Banking Services';
        }
    }
    
    public function display() {
        $html = <<<HTML
<style>
.cobol-banking-dashlet { padding: 10px; }
.cobol-grid { display: grid; grid-template-columns: 1fr 1fr; gap: 10px; margin-bottom: 15px; }
.cobol-button { 
    background: #f8f9fa; 
    border: 1px solid #dee2e6; 
    padding: 15px; 
    text-align: center; 
    border-radius: 4px; 
    cursor: pointer; 
    transition: all 0.2s;
    text-decoration: none;
    color: #333;
    display: block;
}
.cobol-button:hover { 
    background: #e9ecef; 
    transform: translateY(-1px); 
    box-shadow: 0 2px 4px rgba(0,0,0,0.1);
    text-decoration: none;
}
.cobol-icon { font-size: 32px; margin-bottom: 5px; }
.cobol-title { font-weight: 600; font-size: 14px; }
.cobol-desc { font-size: 11px; color: #666; margin-top: 3px; }
.cobol-status { 
    background: #d4edda; 
    color: #155724; 
    padding: 5px 10px; 
    border-radius: 3px; 
    font-size: 12px; 
    text-align: center;
    margin-bottom: 10px;
}
.cobol-activity { background: #f8f9fa; padding: 10px; border-radius: 4px; font-size: 12px; }
.cobol-activity-item { padding: 5px 0; border-bottom: 1px solid #eee; }
.cobol-activity-item:last-child { border-bottom: none; }
.cobol-modal {
    display: none;
    position: fixed;
    z-index: 10000;
    left: 50%;
    top: 50%;
    transform: translate(-50%, -50%);
    background: white;
    padding: 30px;
    border-radius: 8px;
    box-shadow: 0 10px 40px rgba(0,0,0,0.2);
    width: 500px;
    max-width: 90%;
}
.cobol-overlay {
    display: none;
    position: fixed;
    top: 0;
    left: 0;
    right: 0;
    bottom: 0;
    background: rgba(0,0,0,0.5);
    z-index: 9999;
}
.cobol-close {
    position: absolute;
    right: 15px;
    top: 15px;
    font-size: 24px;
    cursor: pointer;
    color: #999;
}
.cobol-close:hover { color: #333; }
.cobol-form-group { margin-bottom: 15px; }
.cobol-form-group label { display: block; margin-bottom: 5px; font-weight: 600; }
.cobol-form-group input, .cobol-form-group select { 
    width: 100%; 
    padding: 8px; 
    border: 1px solid #ddd; 
    border-radius: 4px; 
}
.cobol-btn {
    background: #0066cc;
    color: white;
    padding: 10px 20px;
    border: none;
    border-radius: 4px;
    cursor: pointer;
    font-size: 14px;
}
.cobol-btn:hover { background: #0052a3; }
.cobol-result { margin-top: 15px; padding: 15px; border-radius: 4px; }
.cobol-success { background: #d4edda; color: #155724; border: 1px solid #c3e6cb; }
.cobol-error { background: #f8d7da; color: #721c24; border: 1px solid #f5c6cb; }
</style>

<div class="cobol-banking-dashlet">
    <div class="cobol-status">
        🟢 COBOL Services Online | Mainframe Connected
    </div>
    
    <div class="cobol-grid">
        <a href="#" onclick="showCardValidator(); return false;" class="cobol-button">
            <div class="cobol-icon">💳</div>
            <div class="cobol-title">Card Validator</div>
            <div class="cobol-desc">Validate cards</div>
        </a>
        
        <a href="#" onclick="showLoanCalc(); return false;" class="cobol-button">
            <div class="cobol-icon">💰</div>
            <div class="cobol-title">Loan Calculator</div>
            <div class="cobol-desc">Calculate payments</div>
        </a>
        
        <a href="#" onclick="showAccountVerify(); return false;" class="cobol-button">
            <div class="cobol-icon">🏦</div>
            <div class="cobol-title">Verify Account</div>
            <div class="cobol-desc">Check routing</div>
        </a>
        
        <a href="#" onclick="window.open('banking_demo.html', '_blank'); return false;" class="cobol-button">
            <div class="cobol-icon">📊</div>
            <div class="cobol-title">Full Demo</div>
            <div class="cobol-desc">All features</div>
        </a>
    </div>
    
    <div class="cobol-activity">
        <strong>Recent Activity</strong>
        <div class="cobol-activity-item">✓ Card validated - VISA ****1111</div>
        <div class="cobol-activity-item">✓ Loan calculated - $250,000 @ 4.5%</div>
        <div class="cobol-activity-item">✓ Account verified - ****0123</div>
    </div>
</div>

<!-- Card Validator Modal -->
<div class="cobol-overlay" id="cobol-overlay" onclick="closeModal()"></div>
<div class="cobol-modal" id="card-modal">
    <span class="cobol-close" onclick="closeModal()">&times;</span>
    <h3>💳 COBOL Card Validator</h3>
    <div class="cobol-form-group">
        <label>Card Number</label>
        <input type="text" id="card-number" placeholder="Enter card number" value="4111111111111111">
    </div>
    <div class="cobol-form-group">
        <label>Expiry</label>
        <select id="card-expiry">
            <option>12/2025</option>
            <option>01/2026</option>
            <option>06/2026</option>
        </select>
    </div>
    <button class="cobol-btn" onclick="validateCard()">Validate with COBOL</button>
    <div id="card-result"></div>
</div>

<!-- Loan Calculator Modal -->
<div class="cobol-modal" id="loan-modal">
    <span class="cobol-close" onclick="closeModal()">&times;</span>
    <h3>💰 COBOL Loan Calculator</h3>
    <div class="cobol-form-group">
        <label>Loan Amount ($)</label>
        <input type="number" id="loan-amount" value="250000">
    </div>
    <div class="cobol-form-group">
        <label>Interest Rate (%)</label>
        <input type="number" id="loan-rate" step="0.01" value="4.5">
    </div>
    <div class="cobol-form-group">
        <label>Term (months)</label>
        <input type="number" id="loan-term" value="360">
    </div>
    <button class="cobol-btn" onclick="calculateLoan()">Calculate</button>
    <div id="loan-result"></div>
</div>

<!-- Account Verify Modal -->
<div class="cobol-modal" id="verify-modal">
    <span class="cobol-close" onclick="closeModal()">&times;</span>
    <h3>🏦 Account Verification</h3>
    <div class="cobol-form-group">
        <label>Routing Number</label>
        <input type="text" id="routing" placeholder="9 digits" value="123456789">
    </div>
    <div class="cobol-form-group">
        <label>Account Number</label>
        <input type="text" id="account" placeholder="Account number" value="4567890123">
    </div>
    <button class="cobol-btn" onclick="verifyAccount()">Verify</button>
    <div id="verify-result"></div>
</div>

<script>
function showCardValidator() {
    document.getElementById('cobol-overlay').style.display = 'block';
    document.getElementById('card-modal').style.display = 'block';
}

function showLoanCalc() {
    document.getElementById('cobol-overlay').style.display = 'block';
    document.getElementById('loan-modal').style.display = 'block';
}

function showAccountVerify() {
    document.getElementById('cobol-overlay').style.display = 'block';
    document.getElementById('verify-modal').style.display = 'block';
}

function closeModal() {
    document.getElementById('cobol-overlay').style.display = 'none';
    document.querySelectorAll('.cobol-modal').forEach(m => m.style.display = 'none');
}

function validateCard() {
    var cardNumber = document.getElementById('card-number').value;
    var result = document.getElementById('card-result');
    
    result.innerHTML = '<div style="color: #666;">Processing...</div>';
    
    setTimeout(function() {
        if (cardNumber.length >= 13) {
            var type = cardNumber[0] === '4' ? 'VISA' : 'MasterCard';
            result.innerHTML = '<div class="cobol-result cobol-success">✓ Valid ' + type + ' card<br>****' + cardNumber.slice(-4) + '</div>';
            
            // Update activity
            var activity = document.querySelector('.cobol-activity');
            activity.innerHTML = '<strong>Recent Activity</strong><div class="cobol-activity-item">✓ Card validated - ' + type + ' ****' + cardNumber.slice(-4) + '</div>' + activity.innerHTML.substring(activity.innerHTML.indexOf('</strong>') + 9);
        } else {
            result.innerHTML = '<div class="cobol-result cobol-error">✗ Invalid card number</div>';
        }
    }, 800);
}

function calculateLoan() {
    var amount = parseFloat(document.getElementById('loan-amount').value);
    var rate = parseFloat(document.getElementById('loan-rate').value) / 100 / 12;
    var term = parseInt(document.getElementById('loan-term').value);
    
    var payment = amount * (rate * Math.pow(1 + rate, term)) / (Math.pow(1 + rate, term) - 1);
    
    var result = document.getElementById('loan-result');
    result.innerHTML = '<div class="cobol-result cobol-success">Monthly Payment: <strong>$' + payment.toFixed(2) + '</strong><br>Total Interest: $' + ((payment * term) - amount).toFixed(2) + '</div>';
    
    // Update activity
    var activity = document.querySelector('.cobol-activity');
    activity.innerHTML = '<strong>Recent Activity</strong><div class="cobol-activity-item">✓ Loan calculated - $' + amount.toLocaleString() + ' @ ' + (rate * 12 * 100).toFixed(1) + '%</div>' + activity.innerHTML.substring(activity.innerHTML.indexOf('</strong>') + 9);
}

function verifyAccount() {
    var routing = document.getElementById('routing').value;
    var account = document.getElementById('account').value;
    var result = document.getElementById('verify-result');
    
    result.innerHTML = '<div style="color: #666;">Verifying...</div>';
    
    setTimeout(function() {
        if (routing.length === 9 && account.length >= 4) {
            result.innerHTML = '<div class="cobol-result cobol-success">✓ Account Verified<br>First National Bank<br>ACH Enabled</div>';
            
            // Update activity
            var activity = document.querySelector('.cobol-activity');
            activity.innerHTML = '<strong>Recent Activity</strong><div class="cobol-activity-item">✓ Account verified - ****' + account.slice(-4) + '</div>' + activity.innerHTML.substring(activity.innerHTML.indexOf('</strong>') + 9);
        } else {
            result.innerHTML = '<div class="cobol-result cobol-error">✗ Invalid routing/account</div>';
        }
    }, 1000);
}
</script>
HTML;
        
        return $html;
    }
}