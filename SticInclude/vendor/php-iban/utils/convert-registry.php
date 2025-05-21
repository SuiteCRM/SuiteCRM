<?php

# this script converts the IBAN_registry.txt file's entries to registry.txt format (php-iban's required internal format).

# init
require_once(dirname(dirname(__FILE__)) . '/php-iban.php');
date_default_timezone_set('UTC'); # mutes a warning

# read registry
$data = `iconv -f utf-8 -t ascii --byte-subst="<0x%x>" --unicode-subst="<U+%04X>" 'IBAN_Registry.txt'`;
if($data == '') { die("Couldn't read IBAN_Registry.txt - try downloading from the location described in the REGISTRY-URL file."); }

# print header line
print "country_code|country_name|domestic_example|bban_example|bban_format_swift|bban_format_regex|bban_length|iban_example|iban_format_swift|iban_format_regex|iban_length|bban_bankid_start_offset|bban_bankid_stop_offset|bban_branchid_start_offset|bban_branchid_stop_offset|registry_edition|country_sepa\n";

# break in to lines
$lines = preg_split('/[\r\n]+/',$data);

# display
foreach($lines as $line) {
 # if it's not a blank line, and it's not the header row
 if($line != '' && !preg_match('/SEPA Country/',$line)) {
  # extract individual tab-separated fields
  $bits = explode("\t",$line);
  # remove quotes and superfluous whitespace on fields that have them.
  for($i=0;$i<count($bits);$i++) {
   $bits[$i] = preg_replace('/^"(.*)"$/','$1',$bits[$i]);
   $bits[$i] = preg_replace('/^ */','',$bits[$i]);
   $bits[$i] = preg_replace('/ *$/','',$bits[$i]);
  }
  # assigned fields to named variables
#  print "-------\n";
#  print $line;
#  print "-------\n";
  list($country_name,$country_code,$domestic_example,$bban,$bban_structure,$bban_length,$bban_bi_position,$bban_bi_length,$bban_bi_example,$bban_example,$iban,$iban_structure,$iban_length,$iban_electronic_example,$iban_print_example,$country_sepa,$contact_details) = $bits;
  # sanitise
  $country_code = strtoupper(substr($country_code,0,2));       # sanitise comments away
  $bban_structure = preg_replace('/[:;]/','',$bban_structure); # errors seen in Germany, Hungary entries
  $iban_structure = preg_replace('/, .*$/','',$iban_structure); # duplicates for FO, GL seen in DK
  $iban_electronic_example = preg_replace('/, .*$/','',$iban_electronic_example); # duplicates for FO, GL seen in DK
  if($country_code=='MU') {
   $iban_electronic_example = str_replace(' ','',$iban_electronic_example); # MU example has a spurious space
  }
  if($country_code=='CZ') {
   $iban_electronic_example = preg_replace('/ \w{10,}+$/','',$iban_electronic_example); # extra example for CZ
   $iban_print_example = preg_replace('/^(CZ.. .... .... .... .... ....).*$/','$1',$iban_print_example); # extra example
  }
  if($country_code=='FI') {
   # remove additional example
   $iban_electronic_example = preg_replace('/ or .*$/','',$iban_electronic_example);
   # fix bban example to remove verbosity and match domestic example
   $bban = '12345600000785';
  }
  if($country_code=='KZ') {
   # fix presence of multiline free-text in KZ IBAN structure field
   $iban_structure = '2!a2!n3!n13!c';
  }
  if($country_code=='QA') {
   # fix the lack BBAN structure provision in the TXT format registry
   $bban_structure = '4!a4!n17!c';
   # fix broken IBAN structure provision
   $iban_structure = 'QA2!n4!a4!n17!c';
  }
  if($country_code=='JO') {
   $bban_bi_length=4; # not '4!a' as suggested
  }
  $iban_print_example = preg_replace('/, .*$/','',$iban_print_example); # DK includes FO and GL examples in one record

  # drop leading 2!a in iban structure.
  #  .. should actually be the country code in question
  if(substr($iban_structure,0,3) == '2!a') {
   $iban_structure = $country_code . substr($iban_structure,3);
  }

  # calculate $bban_regex from $bban_structure
  $bban_regex = swift_to_regex($bban_structure);
  # calculate $iban_regex from $iban_structure
  $iban_regex = swift_to_regex($iban_structure);
  print "[DEBUG] got $iban_regex from $iban_structure\n";

 # debugging
 if(true) {
  print "[$country_name ($country_code)]\n";
  print "Domestic account number example: $domestic_example\n";
  print "BBAN structure:                  $bban_structure\n";
  print "BBAN length:                     $bban_length\n";
  print "BBAN bank identifier position:   $bban_bi_position\n";
  print "BBAN bank identifier length:     $bban_bi_length\n";
  print "BBAN bank identifier example:    $bban_bi_example\n";
  print "BBAN example:                    $bban_example\n";
  print "BBAN regex (calculated):         $bban_regex\n";
  print "IBAN structure:                  $iban_structure\n";
  print "IBAN length:                     $iban_length\n";
  print "IBAN electronic format example:  $iban_electronic_example\n";
  print "IBAN print format example:       $iban_print_example\n";
  print "IBAN Regex (calculated):         $iban_regex\n";
  print "SEPA country:                    $country_sepa\n";
  print "Contact details:                 $contact_details\n\n";
 }

  # calculate numeric $bban_length
  $bban_length = preg_replace('/[^\d]/','',$bban_length);
  # calculate numeric $iban_length
  $iban_length = preg_replace('/[^\d]/','',$iban_length);
  # calculate bban_bankid_<start|stop>_offset
  # .... First we have to parse the freetext $bban_bi_position, eg: 
  # Bank Identifier 1-3, Branch Identifier
  # Position 1-2
  # Positions 1-2
  # Positions 1-3
  # Positions 1-3 ;Branch is not available
  # Positions 1-3, Branch identifier
  # Positions 1-3, Branch identifier positions
  # Positions 1-4
  # Positions 1-4, Branch identifier
  # Positions 1-4, Branch identifier positions
  # Positions 1-5
  # Positions 1-5 (positions 1-2 bank identifier; positions 3-5 branch identifier). In case of payment institutions Positions 1-5, Branch identifier positions
  # Positions 1-6,  Branch identifier positions
  # Positions 1-6. First two digits of bank identifier indicate the bank or banking group (For example, 1 or 2 for Nordea, 31 for Handelsbanken, 5 for cooperative banks etc)
  # Positions 1-7
  # Positions 1-8
  # Positions 2-6, Branch identifier positions
  # positions 1-3, Branch identifier positions
  #
  #  ... our algorithm is as follows:
  #   - find all <digit>-<digit> tokens
  preg_match_all('/(\d)-(\d\d?)/',$bban_bi_position,$matches);
  #   - discard overlaps ({1-5,1-2,3-5} becomes {1-2,3-5})
  $tmptokens = array();
  for($j=0;$j<count($matches[0]);$j++) {
   #print "tmptokens was... " . print_r($tmptokens,1) . "\n";
   $from = $matches[1][$j];
   $to = $matches[2][$j];
   #      (if we don't yet have a match starting here, or it goes further,
   #       overwrite the match-from-this-position record)
   if(!isset($tmptokens[$from]) || $to < $tmptokens[$from]) {
    $tmptokens[$from] = $to;
   }
  }
  unset($matches); # done
  #   - assume the token starting from position 1 is the bank identifier
  #     (or, if it does not exist, the token starting from position 2)
  $bban_bankid_start_offset = 0;              # decrement 1 on assignment
  if(isset($tmptokens[1])) {
   $bban_bankid_stop_offset = $tmptokens[1]-1; # decrement 1 on assignment
   unset($tmptokens[1]);
  }
  else {
   $bban_bankid_stop_offset = $tmptokens[2]-1; # decrement 1 on assignment
   unset($tmptokens[2]);
  }
  #   - assume any subsequent token, if present, is the branch identifier.
  $tmpkeys = array_keys($tmptokens);
  $start = array_shift($tmpkeys);
  unset($tmpkeys); # done
  $bban_branchid_start_offset='';
  $bban_branchid_stop_offset='';
  if($start!= '') {
   # we have a branch identifier!
   $bban_branchid_start_offset=$start-1;
   $bban_branchid_stop_offset=$tmptokens[$start]-1;
  }
  else {
   # (note: this codepath occurs for around two thirds of all records)
   # we have not yet found a branch identifier. HOWEVER, we can analyse the
   # structure of the BBAN to determine whether there is more than one
   # remaining non-tiny field (tiny fields on the end of a BBAN typically
   # being checksums) and, if so, assume that the first/shorter one is the
   # branch identifier.
   $reduced_bban_structure = preg_replace('/^\d+![nac]/','',$bban_structure);
#   print "[DEBUG] reduced BBAN structure = $reduced_bban_structure\n";
   $tokens = swift_tokenize($reduced_bban_structure,1);
#   print "[DEBUG] tokens = " + json_encode($tokens,1);
   # discard any tokens of length 1 or 2
   for($t=0;$t<count($tokens[0]);$t++) {
    if($tokens[1][$t] < 3) {
     $tokens['discarded'][$t] = 1;
    }
   }
   # interesting fields are those that are not discarded...
   if(!isset($tokens['discarded'])) {
    $interesting_field_count = count($tokens[0]); }
   else {
    $interesting_field_count = (count($tokens[0])-count($tokens['discarded']));
   }
#   print "[DEBUG] interesting field count = $interesting_field_count\n";
   # ...if we have at least two of them, there's a branchid-type field
   if($interesting_field_count >= 2) {
    # now loop through until we assign the branchid start offset
    # (this occurs just after the first non-discarded field)
    $found=0;
    for($f=0; (($found==0) && ($f<count($tokens[0]))); $f++) {
     # if this is a non-discarded token, of >2 length...
     if((!isset($tokens['discarded'][$f]) || $tokens['discarded'][$f] != 1) && $tokens[1][$f]>2) {
      # ... then assign.
      $pre_offset = $bban_bankid_stop_offset+1; # this is the offset before we reduced the structure to remove the bankid field
      $bban_branchid_start_offset = $pre_offset + $tokens['offset'][$f];
      $bban_branchid_stop_offset  = $pre_offset + $tokens['offset'][$f] + $tokens[1][$f] - 1; # decrement by one on assignment
      $found=1;
     }
    }
   }
  }

  # fix for Jordan
  if($country_code == 'JO') {
   $bban_bankid_start_offset = 0;
   $bban_bankid_stop_offset = 3;
   $bban_branchid_start_offset = 4;
   $bban_branchid_stop_offset = 7;
  }

  # calculate 1=Yes, 0=No for $country_sepa
  # NOTE: This is buggy due to the free inclusion of random text by the registry publishers.
  #       Notably it requires modification for places like Finland and Portugal where these
  #       comments are known to exist.
  if(strtolower($country_sepa)=='yes') { $country_sepa=1; } else { $country_sepa = 0; }
  # set registry edition
  $registry_edition = date('Y-m-d');

  # now prepare generate our registry lines...
  $to_generate = array($country_code=>$country_name);
  if($country_code == 'DK') {
   $to_generate = array('DK'=>$country_name,'FO'=>'Faroe Islands','GL'=>'Greenland');
  }
  elseif($country_code == 'FR') {
   $to_generate = array('FR'=>$country_name,'BL'=>'Saint Barthelemy','GF'=>'French Guyana','GP'=>'Guadelope','MF'=>'Saint Martin (French Part)','MQ'=>'Martinique','RE'=>'Reunion','PF'=>'French Polynesia','TF'=>'French Southern Territories','YT'=>'Mayotte','NC'=>'New Caledonia','PM'=>'Saint Pierre et Miquelon','WF'=>'Wallis and Futuna Islands');
  }

  # output loop
  foreach($to_generate as $country_code=>$country_name) {
   # fixes for fields duplicating country code
   #print "CHECKSUM-BEFORE[$country_code] = $iban_electronic_example\n";
   $iban_electronic_example = iban_set_checksum($country_code .  substr($iban_electronic_example,2));
   #print "CHECKSUM-AFTER[$country_code]  = $iban_electronic_example\n";
   $iban_structure = $country_code . substr($iban_structure,2);
   # step 1
   $iban_regex_fixed = '^' . $country_code;
   $tmp_country_code = substr($iban_regex,1,2);
   #print "[DEBUG] $tmp_country_code\n";
   # route #1 ... here we are dealing with a country code in the string already
   if(preg_match('/^[A-Z][A-Z]$/',$tmp_country_code)) {
    #print "[DEBUG] route #1\n";
    $iban_regex_fixed = $iban_regex_fixed . substr($iban_regex,3);
   }
   # route #2 ... here there is no country code yet present
   else {
    #print "[DEBUG] route #2\n";
    $iban_regex_fixed = $iban_regex_fixed . substr($iban_regex,1);
   }
   #print "[DEBUG] substited '$iban_regex_fixed' for '$iban_regex'\n";
   # output
   print "$country_code|$country_name|$domestic_example|$bban_example|$bban_structure|$bban_regex|$bban_length|$iban_electronic_example|$iban_structure|$iban_regex_fixed|$iban_length|$bban_bankid_start_offset|$bban_bankid_stop_offset|$bban_branchid_start_offset|$bban_branchid_stop_offset|$registry_edition|$country_sepa\n";
  }

 }
}

# swift_to_regex()
#  converts the SWIFT IBAN format specifications to regular expressions
#  eg: 4!n6!n1!n -> ^(\d{4})(\d{6})(\d{1})$
function swift_to_regex($swift) {
 # first find tokens
 $matches = swift_tokenize($swift);
 # now replace bits
 $tr = '^' . $swift . '$';
 # loop through each matched token
 for($i=0;$i<count($matches[0]);$i++) {
  # calculate replacement
  $replacement = '(TOKEN)';
  # type 'n'
  if($matches[3][$i] == 'n') { 
   $replacement = '(\d{length})';
  }
  # type 'c'
  elseif($matches[3][$i] == 'c') {
   $replacement = '([A-Za-z0-9]{length})';
  }
  # type 'a'
  elseif($matches[3][$i] == 'a') {
   $replacement = '([A-Z]{length})';
#' . $matches[1][$i] . '})';
  }
  else {
   print "unknown type: $matches[3][$i]\n";
   exit(1);
  }
  # now add length indicator to the token
  $length = '(LENGTH)';
  if($matches[2][$i] == '!') {
    $length = $matches[1][$i];
  }
  else {
   $length = '1,' . $matches[1][$i];
  }
  $replacement = preg_replace('/length/',$length,$replacement,1);
  # finally, replace the entire token with the replacement
  $tr = preg_replace('/' . $matches[0][$i] . '/',$replacement,$tr,1);
 }
 return $tr;
}

# swift_tokenize()
#  fetch individual tokens in a swift structural string
function swift_tokenize($string,$calculate_offsets=0) {
 preg_match_all('/((?:\d*?[1-2])?\d)(!)?([anc])/',$string,$matches);
 if($calculate_offsets) {
  $current_offset=0;
  for($i=0;$i<count($matches[0]);$i++) {
   $matches['offset'][$i] = $current_offset;
   $current_offset+=$matches[1][$i];
  }
  #print "ANALYSE[raw]: " . join(',',$matches['offset']);
 }
 return $matches;
}

?>
