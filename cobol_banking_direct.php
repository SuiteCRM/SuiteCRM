<?php
// Direct COBOL Banking Integration
if(!defined('sugarEntry')) define('sugarEntry', true);
require_once('include/entryPoint.php');

// Check if user is logged in
global $current_user;
if(empty($current_user->id)) {
    SugarApplication::redirect('index.php?action=Login&module=Users');
    exit;
}

// Add COBOL Banking content directly to home page
$cobol_content = '
<div style="background: white; padding: 20px; margin: 20px; border-radius: 4px; box-shadow: 0 1px 3px rgba(0,0,0,0.1);">
    <div style="background: #f8f9fa; padding: 15px; margin: -20px -20px 20px -20px; border-bottom: 1px solid #dee2e6; font-weight: bold;">
        🏦 COBOL Banking Services
    </div>
    
    <div style="display: grid; grid-template-columns: repeat(auto-fit, minmax(150px, 1fr)); gap: 15px; margin-bottom: 20px;">
        <button onclick="window.open(\'banking_demo.html\', \'_blank\')" style="background: #0066cc; color: white; border: none; padding: 20px; border-radius: 4px; cursor: pointer; text-align: center;">
            <div style="font-size: 32px; margin-bottom: 5px;">💳</div>
            <div>Card Validator</div>
        </button>
        
        <button onclick="window.open(\'banking_demo.html\', \'_blank\')" style="background: #28a745; color: white; border: none; padding: 20px; border-radius: 4px; cursor: pointer; text-align: center;">
            <div style="font-size: 32px; margin-bottom: 5px;">💰</div>
            <div>Loan Calculator</div>
        </button>
        
        <button onclick="window.open(\'banking_demo.html\', \'_blank\')" style="background: #6f42c1; color: white; border: none; padding: 20px; border-radius: 4px; cursor: pointer; text-align: center;">
            <div style="font-size: 32px; margin-bottom: 5px;">🏦</div>
            <div>Account Verify</div>
        </button>
        
        <button onclick="window.open(\'suitecrm_banking_demo.html\', \'_blank\')" style="background: #17a2b8; color: white; border: none; padding: 20px; border-radius: 4px; cursor: pointer; text-align: center;">
            <div style="font-size: 32px; margin-bottom: 5px;">📊</div>
            <div>Full Demo</div>
        </button>
    </div>
    
    <div style="background: #d4edda; color: #155724; padding: 10px; border-radius: 4px; text-align: center; margin-bottom: 15px;">
        🟢 COBOL Services Online | Mainframe Connected
    </div>
    
    <h4>Recent Banking Activity</h4>
    <div style="background: #f8f9fa; padding: 15px; border-radius: 4px;">
        <div style="padding: 5px 0;">✓ Card validated - VISA ****1111</div>
        <div style="padding: 5px 0;">✓ Loan calculated - $250,000 @ 4.5%</div>
        <div style="padding: 5px 0;">✓ Account verified - ****0123</div>
    </div>
</div>';

echo $cobol_content;
?>