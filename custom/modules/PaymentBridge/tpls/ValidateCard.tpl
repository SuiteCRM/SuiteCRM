{literal}
<style>
.payment-validation-form {
    max-width: 600px;
    margin: 20px auto;
    padding: 20px;
    background: #fff;
    border: 1px solid #ddd;
    border-radius: 5px;
}
.validation-result {
    margin-top: 20px;
    padding: 15px;
    border-radius: 5px;
    display: none;
}
.validation-success {
    background-color: #d4edda;
    border-color: #c3e6cb;
    color: #155724;
}
.validation-error {
    background-color: #f8d7da;
    border-color: #f5c6cb;
    color: #721c24;
}
.card-input {
    font-size: 18px;
    letter-spacing: 2px;
}
.loading {
    display: none;
    text-align: center;
    margin: 20px 0;
}
</style>
<script>
function validateCard() {
    var cardNumber = document.getElementById('card_number').value;
    var accountId = document.getElementById('account_id').value;
    
    if (!cardNumber || cardNumber.length !== 16) {
        alert('Please enter a valid 16-digit card number');
        return;
    }
    
    document.getElementById('loading').style.display = 'block';
    document.getElementById('validation-result').style.display = 'none';
    document.getElementById('validate-btn').disabled = true;
    
    $.ajax({
        url: 'index.php?module=PaymentBridge&action=AjaxValidateCard&to_pdf=1',
        type: 'POST',
        data: {
            card_number: cardNumber,
            account_id: accountId
        },
        success: function(response) {
            var result = JSON.parse(response);
            document.getElementById('loading').style.display = 'none';
            document.getElementById('validate-btn').disabled = false;
            
            var resultDiv = document.getElementById('validation-result');
            resultDiv.style.display = 'block';
            
            if (result.success && result.data.valid) {
                resultDiv.className = 'validation-result validation-success';
                resultDiv.innerHTML = '<h3>✓ Card Valid</h3>' +
                    '<p>Card Number: ' + result.data.cardNumber + '</p>' +
                    '<p>Card Type: ' + result.data.cardType + '</p>' +
                    '<p><a href="index.php?module=PaymentBridge&action=DetailView&record=' + 
                    result.record_id + '">View Payment Record</a></p>';
            } else {
                resultDiv.className = 'validation-result validation-error';
                resultDiv.innerHTML = '<h3>✗ Card Invalid</h3>' +
                    '<p>' + (result.error || 'The card number failed validation') + '</p>';
            }
        },
        error: function() {
            document.getElementById('loading').style.display = 'none';
            document.getElementById('validate-btn').disabled = false;
            alert('Error connecting to payment gateway');
        }
    });
}

function formatCardNumber(input) {
    var value = input.value.replace(/\s+/g, '').replace(/[^0-9]/gi, '');
    input.value = value;
}
</script>
{/literal}

<h1>{$MOD.LBL_PAYMENT_VALIDATION}</h1>

<div class="payment-validation-form">
    <form onsubmit="validateCard(); return false;">
        <input type="hidden" id="account_id" value="{$ACCOUNT_ID}" />
        
        {if $ACCOUNT_NAME}
        <div class="form-group">
            <label>Account:</label>
            <strong>{$ACCOUNT_NAME}</strong>
        </div>
        {/if}
        
        <div class="form-group">
            <label for="card_number">Credit Card Number:</label>
            <input type="text" 
                   id="card_number" 
                   class="form-control card-input" 
                   maxlength="16" 
                   placeholder="Enter 16-digit card number"
                   onkeyup="formatCardNumber(this)"
                   required />
            <small class="form-text text-muted">Enter card number without spaces or dashes</small>
        </div>
        
        <div class="form-group">
            <button type="submit" id="validate-btn" class="btn btn-primary">
                {$MOD.LBL_VALIDATE} Card
            </button>
            
            {if $ACCOUNT_ID}
            <a href="index.php?module=Accounts&action=DetailView&record={$ACCOUNT_ID}" 
               class="btn btn-default">
                Back to Account
            </a>
            {/if}
        </div>
    </form>
    
    <div id="loading" class="loading">
        <img src="themes/SuiteP/images/loading.gif" alt="Loading..." />
        <p>Validating card...</p>
    </div>
    
    <div id="validation-result" class="validation-result"></div>
    
    <div style="margin-top: 30px; padding-top: 20px; border-top: 1px solid #ddd;">
        <h4>Test Credit Card Numbers:</h4>
        <ul>
            <li>4532015112830366 - Valid VISA</li>
            <li>5425233430109903 - Valid MasterCard</li>
            <li>4111111111111111 - Valid VISA</li>
        </ul>
    </div>
</div>