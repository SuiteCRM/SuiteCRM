<?php

# Basic test script for the php-iban library
#  - Exit status is 0 in case of success, or the number of failed tests
#    in case of failure

# Engine configuration
#  - first we enable error display
ini_set('display_errors',1);
#  - next we ensure that all errors are displayed
ini_set('error_reporting',E_ALL);

# include the library itself
require_once(dirname(dirname(__FILE__)) . '/php-iban.php');

# display registry contents
#print_r($_iban_registry);

# init
$errors=0;

# Try to validate an invalid IBAN
$iban = "(@#(*@*ZV-This is NOT an IBAN!";
if(verify_iban($iban)) {
 print "ERROR: An invalid IBAN was validated!\n";
 $errors++;
}
print "Hooray! - Invalid IBAN successfully rejected.\n\n";

# Broken IIBAN
$broken_iiban = 'VG96VPVG00000L2345678901';
$suggestions = iban_mistranscription_suggestions($broken_iiban);
if(count($suggestions)) {
 print "Hooray!  Successfully derived '" . implode(',',$suggestions) . "' as likely transcription error source suggestion(s) for the incorrect IBAN $broken_iiban.\n";
}
else {
 print "ERROR: Not able to ascertain suggested transcription error source(s) for $broken_iiban.\n";
 $errors++;
}
print "\n";

# Loop through the registry's examples, validating
foreach($_iban_registry as $country) {

 # get country code
 $countrycode = $country['country'];
 $ianacountrycode = $country['country_iana'];
 $iso3166countrycode = $country['country_iso3166'];

 # start section
 print "[$countrycode (";
 $codes_output=0;
 if($ianacountrycode!='') {
  print "IANA:.$ianacountrycode";
  $codes_output++;
 }
 if($iso3166countrycode!='') {
  if($codes_output>0) { print ", "; }
  print "ISO3166-1 alpha-2:$iso3166countrycode";
 }
 if($codes_output==0) { print "no IANA or ISO3166-1 alpha-2 codes"; }
 print "): " . iban_country_get_country_name($countrycode) . "]\n";

 # output remaining country properties
 print "Is a SEPA member? ";
 if(iban_country_is_sepa($countrycode)) { print "Yes"; } else { print "No"; }
 print ".\n";

 # output EU member state
 print "Is a EU member? ";
 if(iban_country_get_is_eu_member($countrycode)) { print "Yes"; } else { print "No"; }
 print ".\n";

 # central bank
 print "Central Bank: ";
 $central_bank_name = iban_country_get_central_bank_name($countrycode);
 if($central_bank_name!='') {
  print $central_bank_name;
  $central_bank_url = iban_country_get_central_bank_url($countrycode);
  if($central_bank_url!='') {
   print " ($central_bank_url)";
  }
 }
 else {
  print "None.";
 }
 print "\n";

 # output remaining country properties
 print "Is a EU member? ";
 if(iban_country_get_is_eu_member($countrycode)) { print "Yes"; } else { print "No"; }
 print ".\n";

 # parent_registrar
 print "Has own team of bureaucrats? ";
 $parent_registrar = iban_country_get_parent_registrar($countrycode);
 if($parent_registrar!='') {
  print "No (outsources to the wise experts of '" . $parent_registrar . "')\n";
 }
 else {
  print "Yes.\n";
 }

 # official currency
 print "Official currency: ";
 $official_currency = iban_country_get_currency_iso4217($countrycode);
 if($official_currency == '') {
  print "None.";
 }
 else {
  print $official_currency;
 }
 print "\n";

 # get example iban
 $iban = $country['iban_example'];

 # output example iban properties one by one
 print "Example IBAN: " . iban_to_human_format($iban) . "\n";
 print " - country  " . iban_get_country_part($iban) . "\n";
 print " - checksum " . iban_get_checksum_part($iban) . "\n";
 print " - bban     " . iban_get_bban_part($iban) . "\n";
 print " - bank     " . iban_get_bank_part($iban) . "\n";
 print " - branch   " . iban_get_branch_part($iban) . "\n";
 print " - account  " . iban_get_account_part($iban) . "\n";
 $nationalchecksum = iban_get_nationalchecksum_part($iban);
 print " - natcksum " . $nationalchecksum . "\n";

 # if a national checksum was present, validate it
 $supposed_checksum = iban_find_nationalchecksum($iban);
 if($supposed_checksum!='') {
  if($supposed_checksum != $nationalchecksum) {
   print "    (INVALID! Should be '" . $supposed_checksum . "'!)\n";
   exit(1);
  }
  else {
   print "    (National checksum manually validated.)\n";
  }
 }
 # also check 'verify' codepath
 $result = iban_verify_nationalchecksum($iban);
 if($result!='') {
  if($result!=true) {
   print "    (ERROR: iban_verify_nationalchecksum($iban) did not validate!)\n";
   exit(1);
  }
  else {
   print "    (National checksum automatically validated.)\n";
  }
 }
 # also check 'set' codepath
 $fixed_iban = iban_set_nationalchecksum($iban);
 if($fixed_iban != '') {
  if($fixed_iban != $iban) {
   print "    (ERROR: iban_set_nationalchecksum('$iban') returned '$fixed_iban')\n";
   exit(1);
  }
  else {
   print "    (Correction of national checksum functionality validated.)\n";
  }
 }
 
 # output all properties
 #$parts = iban_get_parts($iban);
 #print_r($parts);
 
 # verify
 print "\nChecking validity... ";
 if(verify_iban($iban)) {
  print "IBAN $iban is valid.\n";
 }
 else {
  print "ERROR: IBAN $iban is invalid.\n";
  $correct = iban_set_checksum($iban);
  if($correct == $iban) { 
   print "       (checksum is correct, structure must have issues.)\n";
   $machine_iban = iban_to_machine_format($iban);
   print "        (machine format is: '$machine_iban')\n";
   $country = iban_get_country_part($machine_iban);
   print "        (country is: '$country')\n";
   if(strlen($machine_iban)!=iban_country_get_iban_length($country)) {
    print "        (ERROR: length of '" . strlen($machine_iban) . "' does not match expected length for country's IBAN '" . iban_country_get_iban_length($country) . "'.)";
   }
   $regex = '/'.iban_country_get_iban_format_regex($country).'/';
   if(!preg_match($regex,$machine_iban)) {
    print "        (ERROR: did not match regular expression '" . $regex . "')\n";
   }
  }
  else {
   print "       (correct checksum version would be '" . $correct . "')\n";
  }
  $errors++;
  exit(1);
 }

 print "\n";
}

exit($errors);

?>
