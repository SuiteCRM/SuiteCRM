<?php
/**
 *
 * SugarCRM Community Edition is a customer relationship management program developed by
 * SugarCRM, Inc. Copyright (C) 2004-2013 SugarCRM Inc.
 *
 * SuiteCRM is an extension to SugarCRM Community Edition developed by SalesAgility Ltd.
 * Copyright (C) 2011 - 2024 SalesAgility Ltd.
 *
 * This program is free software; you can redistribute it and/or modify it under
 * the terms of the GNU Affero General Public License version 3 as published by the
 * Free Software Foundation with the addition of the following permission added
 * to Section 15 as permitted in Section 7(a): FOR ANY PART OF THE COVERED WORK
 * IN WHICH THE COPYRIGHT IS OWNED BY SUGARCRM, SUGARCRM DISCLAIMS THE WARRANTY
 * OF NON INFRINGEMENT OF THIRD PARTY RIGHTS.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more
 * details.
 *
 * You should have received a copy of the GNU Affero General Public License along with
 * this program; if not, see http://www.gnu.org/licenses or write to the Free
 * Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
 * 02110-1301 USA.
 *
 * You can contact SugarCRM, Inc. headquarters at 10050 North Wolfe Road,
 * SW2-130, Cupertino, CA 95014, USA. or at email address contact@sugarcrm.com.
 *
 * The interactive user interfaces in modified source and object code versions
 * of this program must display Appropriate Legal Notices, as required under
 * Section 5 of the GNU Affero General Public License version 3.
 *
 * In accordance with Section 7(b) of the GNU Affero General Public License version 3,
 * these Appropriate Legal Notices must retain the display of the "Powered by
 * SugarCRM" logo and "Supercharged by SuiteCRM" logo. If the display of the logos is not
 * reasonably feasible for technical reasons, the Appropriate Legal Notices must
 * display the words "Powered by SugarCRM" and "Supercharged by SuiteCRM".
 */

if (!defined('sugarEntry') || !sugarEntry) {
    die('Not A Valid Entry Point');
}

 #[\AllowDynamicProperties]
 class ListCurrency
 {
     public $focus = null;
     public $list = null;
     public $javascript = '<script>';
     public function lookupCurrencies()
     {
         $this->focus = BeanFactory::newBean('Currencies');
         $this->list = $this->focus->get_full_list('name');
         $this->focus->retrieve('-99');
         if (is_array($this->list)) {
             $this->list = array_merge(array($this->focus), $this->list);
         } else {
             $this->list = array($this->focus);
         }
     }
     public function handleAdd()
     {
         global $current_user;
         if ($current_user->is_admin) {
             if (isset($_POST['edit']) && $_POST['edit'] == 'true' && isset($_POST['name']) && !empty($_POST['name']) && isset($_POST['conversion_rate']) && !empty($_POST['conversion_rate']) && isset($_POST['symbol']) && !empty($_POST['symbol'])) {
                 $currency = BeanFactory::newBean('Currencies');
                 if (isset($_POST['record']) && !empty($_POST['record'])) {
                     $currency->retrieve($_POST['record']);
                 }
                 $currency->name = $_POST['name'];
                 $currency->status = $_POST['status'];
                 $currency->symbol = $_POST['symbol'];
                 $currency->iso4217 = $_POST['iso4217'];
                 $currency->conversion_rate = unformat_number($_POST['conversion_rate']);
                 $currency->save();
                 $this->focus = $currency;
             }
         }
     }

     public function handleUpdate()
     {
         global $current_user;
         if ($current_user->is_admin) {
             if (isset($_POST['id']) && !empty($_POST['id'])&&isset($_POST['name']) && !empty($_POST['name']) && isset($_POST['rate']) && !empty($_POST['rate']) && isset($_POST['symbol']) && !empty($_POST['symbol'])) {
                 $ids = $_POST['id'];
                 $names= $_POST['name'];
                 $symbols= $_POST['symbol'];
                 $rates  = $_POST['rate'];
                 $isos  = $_POST['iso'];
                 $size = is_countable($ids) ? count($ids) : 0;
                 if ($size !== count($names)|| $size !== count($isos) || $size !== count($symbols) || $size !== count($rates)) {
                     return;
                 }

                 $temp = BeanFactory::newBean('Currencies');
                 for ($i = 0; $i < $size; $i++) {
                     $temp->id = $ids[$i];
                     $temp->name = $names[$i];
                     $temp->symbol = $symbols[$i];
                     $temp->iso4217 = $isos[$i];
                     $temp->conversion_rate = $rates[$i];
                     $temp->save();
                 }
             }
         }
     }

     public function getJavascript()
     {
         // wp: DO NOT add formatting and unformatting numbers in here, add them prior to calling these to avoid double calling
         // of unformat number
         return $this->javascript . <<<EOQ
					function get_rate(id){
						return ConversionRates[id];
					}
					
					function ConvertToDollar(amount, rate){
						return amount / rate;
					}
					
					function ConvertFromDollar(amount, rate){
						return amount * rate;
					}
					
					function ConvertRate(id,fields){
						for(var i = 0; i < fields.length; i++){
							fields[i].value = toDecimal(ConvertFromDollar(toDecimal(ConvertToDollar(toDecimal(fields[i].value), lastRate)), ConversionRates[id]));
						}
						lastRate = ConversionRates[id];
					}
					
					function ConvertRateSingle(id,field){
						var temp = field.innerHTML.substring(1, field.innerHTML.length);
						unformattedNumber = unformatNumber(temp, num_grp_sep, dec_sep);

						field.innerHTML = CurrencySymbols[id] + formatNumber(toDecimal(ConvertFromDollar(ConvertToDollar(unformattedNumber, lastRate), ConversionRates[id])), num_grp_sep, dec_sep, 2, 2);
						lastRate = ConversionRates[id];
					}
					
					function CurrencyConvertAll(form){
                        try {
                            var id = form.currency_id.options[form.currency_id.selectedIndex].value;
						    var fields = new Array();

						    for(i in currencyFields){
							    var field = currencyFields[i];
							    if(typeof(form[field]) != 'undefined'){
								    form[field].value = unformatNumber(form[field].value, num_grp_sep, dec_sep);
								    fields.push(form[field]);
							    }
                            }
						    
						    ConvertRate(id, fields);
                            
                            for(i in fields){
							    fields[i].value = formatNumber(fields[i].value, num_grp_sep, dec_sep);
						    }
					                         
                            calculateAllLines();
						} catch (err) {
                            // Do nothing, if we can't find the currency_id field we will just not attempt to convert currencies
                            // This typically only happens in lead conversion and quick creates, where the currency_id field may be named somethnig else or hidden deep inside a sub-form.
                        }
					}
				</script>
EOQ;
     }


     public function getSelectOptions($id = '')
     {
         global $current_user;
         $this->javascript .="var ConversionRates = new Array(); \n";
         $this->javascript .="var CurrencySymbols = new Array(); \n";
         $options = '';
         $this->lookupCurrencies();
         $setLastRate = false;
         if (isset($this->list) && !empty($this->list)) {
             foreach ($this->list as $data) {
                 if ($data->status == 'Active') {
                     if ($id == $data->id) {
                         $options .= '<option value="'. $data->id . '" selected>';
                         $setLastRate = true;
                         $this->javascript .= 'var lastRate = "' . $data->conversion_rate . '";';
                     } else {
                         $options .= '<option value="'. $data->id . '">'	;
                     }
                     $options .= $data->name . ' : ' . $data->symbol;
                     $this->javascript .=" ConversionRates['".$data->id."'] = '".$data->conversion_rate."';\n";
                     $this->javascript .=" CurrencySymbols['".$data->id."'] = '".$data->symbol."';\n";
                 }
             }
             if (!$setLastRate) {
                 $this->javascript .= 'var lastRate = "1";';
             }
         }
         return $options;
     }
     public function getTable()
     {
         global $sugar_config;
         $this->lookupCurrencies();
         $usdollar = translate('LBL_US_DOLLAR');
         $currency = translate('LBL_CURRENCY');
         $currency_sym = $sugar_config['default_currency_symbol'];
         $conv_rate = translate('LBL_CONVERSION_RATE');
         $add = translate('LBL_ADD');
         $delete = translate('LBL_DELETE');
         $update = translate('LBL_UPDATE');

         $form = $html = "<br><table cellpadding='0' cellspacing='0' border='0'  class='tabForm'><tr><td><tableborder='0' cellspacing='0' cellpadding='0'>";
         $form .= <<<EOQ
					<form name='DeleteCurrency' action='index.php' method='post'><input type='hidden' name='action' value='{$_REQUEST['action']}'>
					<input type='hidden' name='module' value='{$_REQUEST['module']}'><input type='hidden' name='deleteCur' value=''></form>

					<tr><td><B>$currency</B></td><td><B>ISO 4217</B>&nbsp;</td><td><B>$currency_sym</B></td><td colspan='2'><B>$conv_rate</B></td></tr>
					<tr><td>$usdollar</td><td>USD</td><td>$</td><td colspan='2'>1</td></tr>
					<form name="UpdateCurrency" action="index.php" method="post"><input type='hidden' name='action' value='{$_REQUEST['action']}'>
					<input type='hidden' name='module' value='{$_REQUEST['module']}'>
EOQ;
         if (isset($this->list) && !empty($this->list)) {
             foreach ($this->list as $data) {
                 $form .= '<tr><td>'.$data->iso4217. '<input type="hidden" name="iso[]" value="'.$data->iso4217.'"></td><td><input type="hidden" name="id[]" value="'.$data->id.'">'.$data->name. '<input type="hidden" name="name[]" value="'.$data->name.'"></td><td>'.$data->symbol. '<input type="hidden" name="symbol[]" value="'.$data->symbol.'"></td><td>'.$data->conversion_rate.'&nbsp;</td><td><input type="text" name="rate[]" value="'.$data->conversion_rate.'"><td>&nbsp;<input type="button" name="delete" class="button" value="'.$delete.'" onclick="document.forms[\'DeleteCurrency\'].deleteCur.value=\''.$data->id.'\';document.forms[\'DeleteCurrency\'].submit();"> </td></tr>';
             }
         }
         $form .= <<<EOQ
					<tr><td></td><td></td><td></td><td></td><td></td><td>&nbsp;<input type='submit' name='Update' value='$update' class='button'></TD></form> </td></tr>
					<tr><td colspan='3'><br></td></tr>
					<form name="AddCurrency" action="index.php" method="post">
					<input type='hidden' name='action' value='{$_REQUEST['action']}'>
					<input type='hidden' name='module' value='{$_REQUEST['module']}'>
					<tr><td><input type = 'text' name='addname' value=''>&nbsp;</td><td><input type = 'text' name='addiso' size='3' maxlength='3' value=''>&nbsp;</td><td><input type = 'text' name='addsymbol' value=''></td><td colspan='2'>&nbsp;<input type ='text' name='addrate'></td><td>&nbsp;<input type='submit' name='Add' value='$add' class='button'></td></tr>
					</form></table></td></tr></table>
EOQ;
         return $form;
     }

     public function setCurrencyFields($fields)
     {
         $json = getJSONobj();
         $this->javascript .= 'var currencyFields = ' . $json->encode($fields) . ";\n";
     }
 }

//$lc = new ListCurrency();
//$lc->handleDelete();
//$lc->handleAdd();
//$lc->handleUpdate();
//echo '<select>'. $lc->getSelectOptions() . '</select>';
//echo $lc->getTable();
