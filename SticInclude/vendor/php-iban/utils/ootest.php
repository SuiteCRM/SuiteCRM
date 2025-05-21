<?php

# Basic test script for the object-oriented version of the php-iban library
#  - Exit status is 0 in case of success, or the number of failed tests
#    in case of failure

# Engine configuration
#  - first we enable error display
ini_set('display_errors',1);
#  - next we ensure that all errors are displayed
ini_set('error_reporting',E_ALL);

# include the object oriented version of the library itself
require_once(dirname(dirname(__FILE__)) . '/oophp-iban.php');

# display registry contents
#print_r($_iban_registry);

# init
$errors=0;

# Try to validate an invalid IBAN
$iban = "(@#(*@*ZV-This is NOT an IBAN!";
$myIban = new PHP_IBAN\IBAN($iban);
if($myIban->verify()) {
 print "ERROR: An invalid IBAN was validated!\n";
 $errors++;
}
print "Hooray! - Invalid IBAN successfully rejected.\n\n";

# Broken IIBAN
$broken_iiban = 'VG96VPVG00000L2345678901';
$myIban = new PHP_IBAN\IBAN($broken_iiban);
$suggestions = $myIban->MistranscriptionSuggestions();
if(count($suggestions)) {
 print "Hooray!  Successfully derived '" . implode(',',$suggestions) . "' as likely transcription error source suggestion(s) for the incorrect IBAN $broken_iiban.\n";
}
else {
 print "ERROR: Not able to ascertain suggested transcription error source(s) for $broken_iiban.\n";
 $errors++;
}
print "\n";

# Get list of countries
$countries = $myIban->Countries();

# Loop through the registry's examples, validating
foreach($countries as $countrycode) {

 # instantiate
 $myCountry = new PHP_IBAN\IBANCountry($countrycode);

 # get country code
 $ianacountrycode = $myCountry->IANA();
 $iso3166countrycode = $myCountry->ISO3166();

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
 print "): " .  $myCountry->Name() . "]\n";

 # output remaining country properties
 print "Is a SEPA member? ";
 if($myCountry->IsSEPA()) { print "Yes"; } else { print "No"; }
 print ".\n";

 # output EU member state
 print "Is a EU member? ";
 if($myCountry->IsEuMember()) { print "Yes"; } else { print "No"; }
 print ".\n";

 # central bank
 print "Central Bank: ";
 $central_bank_name = $myCountry->CentralBankName();
 if($central_bank_name!='') {
  print $central_bank_name;
  $central_bank_url = $myCountry->CentralBankURL();
  if($central_bank_url!='') {
   print " ($central_bank_url)";
  }
 }
 else {
  print "None.";
 }
 print "\n";

 # parent_registrar
 print "Has own team of bureaucrats? ";
 $parent_registrar = $myCountry->ParentRegistrar();
 if($parent_registrar!='') {
  print "No (outsources to the wise experts of '" . $parent_registrar . "')\n";
 }
 else {
  print "Yes.\n";
 }

 # official currency
 print "Official currency: ";
 $official_currency = $myCountry->CurrencyISO4217();
 if($official_currency == '') {
  print "None.";
 }
 else {
  print $official_currency;
 }
 print "\n";

 # get example iban
 $myIban = new PHP_IBAN\IBAN($myCountry->IBANExample());

 # output example iban properties one by one
 print "Example IBAN: " . $myIban->HumanFormat() . "\n";
 print " - country  " . $myIban->Country() . "\n";
 print " - checksum " . $myIban->Checksum() . "\n";
 print " - bban     " . $myIban->BBAN() . "\n";
 print " - bank     " . $myIban->Bank() . "\n";
 print " - branch   " . $myIban->Branch() . "\n";
 print " - account  " . $myIban->Account() . "\n";
 $nationalchecksum = $myIban->NationalChecksum();
 print " - natcksum " . $nationalchecksum . "\n";

 # if a national checksum was present, validate it
 $supposed_checksum = $myIban->FindNationalChecksum();
 if($supposed_checksum!='') {
  if($supposed_checksum != $nationalchecksum) {
   print "    (INVALID! Should be '" . $supposed_checksum . "'!)\n";
   exit(1);
  }
  else {
   print "    (National checksum manually validated.)\n";
  }
  # also check 'verify' codepath
  if(!$myIban->VerifyNationalChecksum()) {
   print "    (ERROR: VerifyNationalChecksum($iban) did not validate!)\n";
   exit(1);
  }
  else {
   print "    (National checksum automatically validated.)\n";
  }
  # also check 'set' codepath
  $myIban->SetNationalChecksum();
  if($myCountry->IBANExample() != $myIban->iban) {
   print "    (ERROR: iban_set_nationalchecksum('" . $myCountry->IBANExample() . "') returned '" . $myIban->iban . "')\n";
   exit(1);
  }
  else {
   print "    (Correction of national checksum functionality validated.)\n";
  }
 }

 # output all properties
 #$parts = $myIban->Parts();
 #print_r($parts);
 
 # verify
 print "\nChecking validity... ";
 if($myIban->Verify()) {
  print "IBAN $myIban->iban is valid.\n";
 }
 else {
  print "ERROR: IBAN $myIban->iban is invalid.\n";
  $correct = $myIban->SetChecksum();
  if($correct == $iban) {
   print "       (checksum is correct, structure must have issues.)\n";
   $machine_iban = $myIban->MachineFormat();
   print "        (machine format is: '$machine_iban')\n";
   $country = $myIban->Country();
   print "        (country is: '$country')\n";
   $myCountry = new PHP_IBAN\IBANCountry($country);
   if(strlen($machine_iban)!=$myCountry->IBANLength()) {
    print "        (ERROR: length of '" . strlen($machine_iban) . "' does not match expected length for country's IBAN '" . $myCountry->IBANLength() . "'.)";
   }
   $regex = '/'.$myCountry->IBANFormatRegex().'/';
   if(!preg_match($regex,$machine_iban)) {
    print "        (ERROR: did not match regular expression '" . $regex . "')\n";
   }
  }
  else {
   print "       (correct checksum version would be '" . $correct . "')\n";
  }
  $errors++;
 }

 print "\n";
}

exit($errors);

?>
