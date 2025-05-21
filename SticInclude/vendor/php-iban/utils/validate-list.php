<?php

error_reporting(E_ALL);

require_once(dirname(__FILE__) . '/../php-iban.php');

if(!isset($argv[1]) || $argv[1]=='-h' || $argv[1]=='--help') {
 usage();
}

$list_file = $argv[1];
$errors=0;

if(!($raw_list = file_get_contents($list_file))) {
 print "Error opening list file '$list_file'.\n";
 exit(1);
}

$list = preg_split("/[\r\n]+/",$raw_list);

$results = array();
foreach($list as $iban) {
 if($iban!='') {
  # let's check it
  print $iban . " ... ";
  if(!verify_iban($iban)) {
   print "FAILED";
########## try to provide better output #############
   $iban = iban_to_machine_format($iban);
   $country = iban_get_country_part($iban);
   $observed_length = strlen($iban);
   $expected_length = iban_country_get_iban_length($country);
   if($observed_length!=$expected_length) {
    print " (length $observed_length does not match expected length $expected_length for country $country)";
   }
   $checksum = iban_get_checksum_part($iban);
   if(!iban_verify_checksum($iban)) {
    print " (checksum $checksum invalid)";
   }
   $regex = '/'.iban_country_get_iban_format_regex($country).'/';
   if(!preg_match($regex,$iban)) {
    print " (does not match regex $regex for country $country)";
   }
####################################################
   $errors++;
   $suggestions = iban_mistranscription_suggestions($iban);
   if(is_array($suggestions)) {
    if(count($suggestions)==1) {
     print " (you meant '" . $suggestions[0] . "', right?)";
    }
    elseif(count($suggestions)>1) {
     print " (perhaps ";
     $done=0;
     foreach($suggestions as $suggestion) {
      if($done>0) { print ', or '; }
      print "'" . $suggestion . "'";
      $done++;
     }
     print "?)";
    }
   }
  }
  else {
   print "ok";
   $result = iban_verify_nationalchecksum($iban);
   if($result==='') {
    print " (no national checksum)";
   }
   elseif($result!=true) {
    print " (but national checksum FAILED! ";
    print "expected '" . iban_find_nationalchecksum($iban) . "', contains '" . iban_get_nationalchecksum_part($iban) . "'. bank code is '".iban_get_bank_part($iban)."')";
    $errors++;
   }
   #$parts = iban_get_parts($iban);
   #foreach($parts as $name=>$value) {
   # print "    $name: $value\n";
   #}
  }
  print "\n";
 }
}

if($errors==0) { exit(0); }
exit(10+$errors);

function usage() {
 global $argv;
 print $argv[0] . " <list-file>\n";
 exit(1);
}

?>
