<?php
require_once(dirname(dirname(__FILE__)) . '/php-iban.php');
$ibans = [];
foreach (glob(dirname(__FILE__) . "/example-ibans/*") as $file) {
 $ibans += file($file);
}
foreach($ibans as $iban) {
 $iban = iban_to_machine_format($iban);
 print iban_to_human_format($iban) . "\n";
 print iban_to_obfuscated_format($iban) . "\n";
}
?>
