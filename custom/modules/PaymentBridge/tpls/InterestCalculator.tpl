{literal}
<style>
.interest-calculator {
    max-width: 800px;
    margin: 20px auto;
    padding: 20px;
    background: #fff;
    border: 1px solid #ddd;
    border-radius: 5px;
}
.calculator-form {
    display: grid;
    grid-template-columns: 1fr 1fr;
    gap: 20px;
}
.form-section {
    padding: 15px;
    background: #f8f9fa;
    border-radius: 5px;
}
.calculation-result {
    margin-top: 20px;
    padding: 20px;
    background: #e8f4f8;
    border: 1px solid #b8e0e8;
    border-radius: 5px;
    display: none;
}
.result-grid {
    display: grid;
    grid-template-columns: repeat(3, 1fr);
    gap: 15px;
    margin-top: 15px;
}
.result-item {
    text-align: center;
    padding: 10px;
    background: #fff;
    border-radius: 5px;
}
.result-value {
    font-size: 24px;
    font-weight: bold;
    color: #2c5282;
}
.payment-plans {
    margin-top: 20px;
    display: none;
}
.plan-option {
    padding: 15px;
    margin: 10px 0;
    background: #f0f0f0;
    border-radius: 5px;
    cursor: pointer;
}
.plan-option:hover {
    background: #e0e0e0;
}
</style>
<script>
function loadCardData() {
    var select = document.getElementById('recent_card');
    var selectedOption = select.options[select.selectedIndex];
    
    if (selectedOption.value) {
        document.getElementById('card_number').value = selectedOption.getAttribute('data-card');
        document.getElementById('balance').value = selectedOption.getAttribute('data-balance');
        document.getElementById('record_id').value = selectedOption.value;
    }
}

function calculateInterest() {
    var cardNumber = document.getElementById('card_number').value;
    var balance = document.getElementById('balance').value;
    var recordId = document.getElementById('record_id').value;
    
    if (!cardNumber || !balance) {
        alert('Please enter card number and balance');
        return;
    }
    
    document.getElementById('calculate-btn').disabled = true;
    
    $.ajax({
        url: 'index.php?module=PaymentBridge&action=AjaxCalculateInterest&to_pdf=1',
        type: 'POST',
        data: {
            card_number: cardNumber,
            balance: balance,
            record_id: recordId
        },
        success: function(response) {
            var result = JSON.parse(response);
            document.getElementById('calculate-btn').disabled = false;
            
            if (result.success) {
                showCalculationResult(result.data);
                generatePaymentPlans(result.data.currentBalance);
            } else {
                alert('Error: ' + result.error);
            }
        },
        error: function() {
            document.getElementById('calculate-btn').disabled = false;
            alert('Error calculating interest');
        }
    });
}

function showCalculationResult(data) {
    document.getElementById('current-balance-result').textContent = '$' + data.currentBalance.toFixed(2);
    document.getElementById('apr-result').textContent = data.apr + '%';
    document.getElementById('monthly-rate-result').textContent = data.monthlyRate + '%';
    document.getElementById('interest-charge-result').textContent = '$' + data.interestCharge.toFixed(2);
    document.getElementById('new-balance-result').textContent = '$' + data.newBalance.toFixed(2);
    document.getElementById('min-payment-result').textContent = '$' + Math.max(25, data.newBalance * 0.03).toFixed(2);
    
    document.getElementById('calculation-result').style.display = 'block';
    document.getElementById('payment-plans').style.display = 'block';
}

function generatePaymentPlans(balance) {
    var plansHtml = '';
    var plans = [
        {months: 3, rate: 0.02, label: '3 Months (2% fee)'},
        {months: 6, rate: 0.05, label: '6 Months (5% fee)'},
        {months: 12, rate: 0.10, label: '12 Months (10% fee)'}
    ];
    
    plans.forEach(function(plan) {
        var total = balance * (1 + plan.rate);
        var monthly = total / plan.months;
        
        plansHtml += '<div class="plan-option" onclick="selectPaymentPlan(' + 
            plan.months + ', ' + total.toFixed(2) + ', ' + monthly.toFixed(2) + ')">' +
            '<h4>' + plan.label + '</h4>' +
            '<p>Monthly Payment: <strong>$' + monthly.toFixed(2) + '</strong></p>' +
            '<p>Total Cost: $' + total.toFixed(2) + '</p>' +
            '</div>';
    });
    
    document.getElementById('plans-container').innerHTML = plansHtml;
}

function selectPaymentPlan(months, total, monthly) {
    if (confirm('Create a ' + months + '-month payment plan with monthly payments of $' + monthly + '?')) {
        var recordId = document.getElementById('record_id').value;
        var balance = document.getElementById('balance').value;
        
        $.ajax({
            url: 'index.php?module=PaymentBridge&action=CreatePaymentPlan&to_pdf=1',
            type: 'POST',
            data: {
                record_id: recordId,
                months: months,
                amount: balance
            },
            success: function(response) {
                var result = JSON.parse(response);
                if (result.success) {
                    alert('Payment plan created successfully!');
                    window.location.href = 'index.php?module=PaymentBridge&action=DetailView&record=' + recordId;
                } else {
                    alert('Error creating payment plan: ' + result.error);
                }
            }
        });
    }
}
</script>
{/literal}

<h1>{$MOD.LBL_INTEREST_CALCULATION}</h1>

<div class="interest-calculator">
    <div class="calculator-form">
        <div class="form-section">
            <h3>Card Information</h3>
            
            <div class="form-group">
                <label>Select Recent Card:</label>
                <select id="recent_card" class="form-control" onchange="loadCardData()">
                    <option value="">-- Select Card --</option>
                    {foreach from=$RECENT_CARDS item=card}
                    <option value="{$card.id}" 
                            data-card="{$card.card_number_masked}"
                            data-balance="{$card.current_balance}"
                            data-apr="{$card.apr}">
                        {$card.card_number_masked} - {$card.name}
                    </option>
                    {/foreach}
                </select>
            </div>
            
            <div class="form-group">
                <label>Card Number:</label>
                <input type="text" id="card_number" class="form-control" 
                       placeholder="Enter or select card number" />
                <input type="hidden" id="record_id" />
            </div>
        </div>
        
        <div class="form-section">
            <h3>Balance Information</h3>
            
            <div class="form-group">
                <label>Current Balance:</label>
                <input type="number" id="balance" class="form-control" 
                       step="0.01" placeholder="0.00" />
            </div>
            
            <div class="form-group">
                <button type="button" id="calculate-btn" class="btn btn-primary" 
                        onclick="calculateInterest()">
                    {$MOD.LBL_CALCULATE} Interest
                </button>
            </div>
        </div>
    </div>
    
    <div id="calculation-result" class="calculation-result">
        <h3>Interest Calculation Results</h3>
        
        <div class="result-grid">
            <div class="result-item">
                <div class="result-label">Current Balance</div>
                <div class="result-value" id="current-balance-result">$0.00</div>
            </div>
            
            <div class="result-item">
                <div class="result-label">APR</div>
                <div class="result-value" id="apr-result">0%</div>
            </div>
            
            <div class="result-item">
                <div class="result-label">Monthly Rate</div>
                <div class="result-value" id="monthly-rate-result">0%</div>
            </div>
            
            <div class="result-item">
                <div class="result-label">Interest Charge</div>
                <div class="result-value" id="interest-charge-result">$0.00</div>
            </div>
            
            <div class="result-item">
                <div class="result-label">New Balance</div>
                <div class="result-value" id="new-balance-result">$0.00</div>
            </div>
            
            <div class="result-item">
                <div class="result-label">Minimum Payment</div>
                <div class="result-value" id="min-payment-result">$0.00</div>
            </div>
        </div>
    </div>
    
    <div id="payment-plans" class="payment-plans">
        <h3>Payment Plan Options</h3>
        <p>Select a payment plan to spread this balance over multiple months:</p>
        <div id="plans-container"></div>
    </div>
</div>