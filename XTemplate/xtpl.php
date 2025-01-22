<?php

/*

Modification information for LGPL compliance
Stas 2010-12-20 Added 'VERSION_MARK' to templates

r56990 - 2010-06-16 13:05:36 -0700 (Wed, 16 Jun 2010) - kjing - snapshot "Mango" svn branch to a new one for GitHub sync

r56989 - 2010-06-16 13:01:33 -0700 (Wed, 16 Jun 2010) - kjing - defunt "Mango" svn dev branch before github cutover

r55980 - 2010-04-19 13:31:28 -0700 (Mon, 19 Apr 2010) - kjing - create Mango (6.1) based on windex

r51719 - 2009-10-22 10:18:00 -0700 (Thu, 22 Oct 2009) - mitani - Converted to Build 3  tags and updated the build system

r51634 - 2009-10-19 13:32:22 -0700 (Mon, 19 Oct 2009) - mitani - Windex is the branch for Sugar Sales 1.0 development

r50375 - 2009-08-24 18:07:43 -0700 (Mon, 24 Aug 2009) - dwong - branch kobe2 from tokyo r50372

r42807 - 2008-12-29 11:16:59 -0800 (Mon, 29 Dec 2008) - dwong - Branch from trunk/sugarcrm r42806 to branches/tokyo/sugarcrm

r36874 - 2008-06-19 12:09:05 -0700 (Thu, 19 Jun 2008) - roger - bug 22568: use md5 of unique key and version for js key.

r32524 - 2008-03-06 15:51:02 -0800 (Thu, 06 Mar 2008) - dwong - Fix incorrect encoding on source code caused by IDE, e.g.
utils.php r3048
InboundEmail.php r17199
header.php r13729

r30876 - 2008-01-09 19:01:57 -0800 (Wed, 09 Jan 2008) - majed - initial check in for instances

r29571 - 2007-11-13 10:49:09 -0800 (Tue, 13 Nov 2007) - eddy - Bug 17113
Added check for array element prior to accessing it.
XTemplate/xtpl.php

r26822 - 2007-09-18 10:20:27 -0700 (Tue, 18 Sep 2007) - tswicegood - Refactor a bunch of the loops and such.
This code doesn't have a future in Sugar, but there are some legacy
areas that still rely on it.  Refactoring these few areas cuts its impact
on Sugar's main page by 40% (6.14% to 3.72%).

r26819 - 2007-09-18 10:07:01 -0700 (Tue, 18 Sep 2007) - tswicegood - Reduces this execution time relatively by 25%

r25238 - 2007-08-07 15:40:32 -0700 (Tue, 07 Aug 2007) - dwheeler - Bug 14129. Removed field from vardefs as it is no longer used, and should not be visible from mass update.

r18355 - 2006-12-05 17:00:55 -0800 (Tue, 05 Dec 2006) - jenny - Bug 10292 - checking to see if we actually have an array before setting the array values.

r13627 - 2006-05-31 11:01:53 -0700 (Wed, 31 May 2006) - majed - name change

r12024 - 2006-03-09 23:42:27 -0800 (Thu, 09 Mar 2006) - majed - fixes bugs 4449 5050 4063 4976 4770

r11291 - 2006-01-22 10:41:45 -0800 (Sun, 22 Jan 2006) - andrew - Removed the 'Log' CVS keyword.

r10797 - 2005-12-21 18:10:38 -0800 (Wed, 21 Dec 2005) - wayne - sugar_version and js_custom_version xtpl assignment now in xtpl.php

r9351 - 2005-11-15 15:39:37 -0800 (Tue, 15 Nov 2005) - andrew - Added another check for the $focus that needs to be in for PHP 5.0.3.

r9270 - 2005-11-11 15:08:19 -0800 (Fri, 11 Nov 2005) - majed - Adds support for emails email marketing and email templates

r8555 - 2005-10-19 12:26:13 -0700 (Wed, 19 Oct 2005) - majed - adds initial acl support

r8508 - 2005-10-17 17:23:04 -0700 (Mon, 17 Oct 2005) - majed - adds initial acl support

r5820 - 2005-06-21 14:22:24 -0700 (Tue, 21 Jun 2005) - majed - fixes issues with nusoap and with custom fields

r4920 - 2005-04-29 00:38:19 -0700 (Fri, 29 Apr 2005) - jacob - Preventing conversion of array to string.

r4743 - 2005-04-27 00:57:27 -0700 (Wed, 27 Apr 2005) - jacob - Adding support for "parsing" sections that do not exist in HTML.  This provides backwards compatibility for old HTML files with new PHP files.

r2016 - 2004-12-28 15:19:29 -0800 (Tue, 28 Dec 2004) - majed - added a function to scan through a block checking for a given variable

r1228 - 2004-10-20 02:09:09 -0700 (Wed, 20 Oct 2004) - lam - update

r1211 - 2004-10-19 21:55:03 -0700 (Tue, 19 Oct 2004) - lam - update

r730 - 2004-09-09 20:14:02 -0700 (Thu, 09 Sep 2004) - sugarjacob - Cleaning up blanks

r462 - 2004-08-25 17:43:37 -0700 (Wed, 25 Aug 2004) - sugarmsi - added an exists method to check if a block exists in a template

r397 - 2004-08-08 02:28:36 -0700 (Sun, 08 Aug 2004) - sugarjacob - Fix: XTemplate changed to use <?php script declarations

r297 - 2004-07-31 15:13:23 -0700 (Sat, 31 Jul 2004) - sugarjacob - Removing default setting of template language arrays.

r295 - 2004-07-31 14:37:38 -0700 (Sat, 31 Jul 2004) - sugarjacob - Adding code to automatically assign the language strings to every template created.

r268 - 2004-07-16 01:21:57 -0700 (Fri, 16 Jul 2004) - sugarjacob - Changing the XTemplate replacement mechanism to allow for '$' in the text being substituted.

r80 - 2004-06-11 16:39:47 -0700 (Fri, 11 Jun 2004) - sugarjacob - Fixing issue with a variable not being an array in some cases.

r78 - 2004-06-11 16:34:17 -0700 (Fri, 11 Jun 2004) - sugarjacob - Removing errors or notices about invalid indexs.

r3 - 2004-05-26 22:30:56 -0700 (Wed, 26 May 2004) - sugarjacob - Moving project to SourceForge.


*/



#[\AllowDynamicProperties]
class XTemplate {

/*
	xtemplate class 0.2.4-3
	html generation with templates - fast & easy
	copyright (c) 2000 barnabás debreceni [cranx@users.sourceforge.net]
	code optimization by Ivar Smolin <okul@linux.ee> 14-march-2001
	latest stable & CVS version always available @ http://sourceforge.net/projects/xtpl

	tested with php 3.0.11 and 4.0.4pl1

	This program is free software; you can redistribute it and/or
	modify it under the terms of the GNU Lesser General Public License
	version 2.1 as published by the Free Software Foundation.

	This library is distributed in the hope that it will be useful,
	but WITHOUT ANY WARRANTY; without even the implied warranty of
	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
	GNU Lesser General Public License for more details at
	http://www.gnu.org/copyleft/lgpl.html

	You should have received a copy of the GNU General Public License
	along with this program; if not, write to the Free Software
	Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.


*/

/***[ variables ]***********************************************************/

public $filecontents="";								/* raw contents of template file */
public $blocks=array();								/* unparsed blocks */
public $parsed_blocks=array();					/* parsed blocks */
public $block_parse_order=array();			/* block parsing order for recursive parsing (sometimes reverse:) */
public $sub_blocks=array();						/* store sub-block names for fast resetting */
public $VARS=array();									/* variables array */
public $alternate_include_directory = "";

public $file_delim="/\{FILE\s*\"([^\"]+)\"\s*\}/m";  /* regexp for file includes */
public $block_start_delim="<!-- ";			/* block start delimiter */
public $block_end_delim="-->";					/* block end delimiter */
public $block_start_word="BEGIN:";			/* block start word */
public $block_end_word="END:";					/* block end word */

/* this makes the delimiters look like: <!-- BEGIN: block_name --> if you use my syntax. */

public $NULL_STRING=array(""=>"");				/* null string for unassigned vars */
public $NULL_BLOCK=array(""=>"");	/* null string for unassigned blocks */
public $mainblock="";
public $ERROR="";
public $AUTORESET=1;										/* auto-reset sub blocks */

/***[ constructor ]*********************************************************/

public function __construct ($file, $alt_include = "", $mainblock="main") {
	$this->alternate_include_directory = $alt_include;
	$this->mainblock=$mainblock;
	$this->filecontents=$this->r_getfile($file);	/* read in template file */
	$this->blocks=$this->maketree($this->filecontents,$mainblock);	/* preprocess some stuff */
	//$this->scan_globals();
}

/***************************************************************************/
/***[ public stuff ]********************************************************/
/***************************************************************************/


/***[ assign ]**************************************************************/
/*
	assign a variable
*/

public function assign ($name,$val="") {
	if (is_array($name)) {
		foreach ($name as $k => $v) {
			$this->VARS[$k] = $v;
		}
	} else {
		$this->VARS[$name]=$val;
	}
}

public function append ($varname, $name,$val="") {
	if(!isset($this->VARS[$varname])){
		$this->VARS[$varname] = array();
	}
   if(is_array($this->VARS[$varname])){
       $this->VARS[$varname][$name] = $val;
    }
}

/***[ parse ]***************************************************************/
/*
	parse a block
*/

public function parse ($bname) {
	global $sugar_version, $sugar_config;

	$this->assign('SUGAR_VERSION', $GLOBALS['js_version_key']);
	$this->assign('JS_CUSTOM_VERSION', $sugar_config['js_custom_version']);
    $this->assign('VERSION_MARK', getVersionedPath(''));

	if(empty($this->blocks[$bname]))
		return;

	$copy=$this->blocks[$bname];
	if (!isset($this->blocks[$bname]))
		$this->set_error ("parse: blockname [$bname] does not exist");
	preg_match_all("/\{([A-Za-z0-9\._]+?)}/",(string) $this->blocks[$bname],$var_array);
	$var_array=$var_array[1];
	foreach ($var_array as $k => $v) {
		$sub=explode(".",$v);
		if ($sub[0]=="_BLOCK_") {
			unset($sub[0]);
			$bname2=implode(".",$sub);

			if(isset($this->parsed_blocks[$bname2]))
			{
				$var=$this->parsed_blocks[$bname2];
			}
			else
			{
				$var = null;
			}

			$nul=(!isset($this->NULL_BLOCK[$bname2])) ? $this->NULL_BLOCK[""] : $this->NULL_BLOCK[$bname2];
			$var=(empty($var))?$nul:trim($var);
			// Commented out due to regular expression issue with '$' in replacement string.
			//$copy=preg_replace("/\{".$v."\}/","$var",$copy);
			// This should be faster and work better for '$'
			$copy=str_replace("{".$v."}",$var,(string) $copy);
		} else {
			$var=$this->VARS;

			foreach ($sub as $k1 => $v1)
			{
				if(is_array($var) && isset($var[$v1]))
				{
					$var=$var[$v1];
				}
				else
				{
					$var = null;
				}
			}

			$nul=(!isset($this->NULL_STRING[$v])) ? ($this->NULL_STRING[""]) : ($this->NULL_STRING[$v]);
			$var=(!isset($var))?$nul:$var;
			// Commented out due to regular expression issue with '$' in replacement string.
			//$copy=preg_replace("/\{$v\}/","$var",$copy);
			// This should be faster and work better for '$'

			// this was periodically returning an array to string conversion error....
			if(!is_array($var))
			{
				$copy=str_replace("{".$v."}",$var,(string) $copy);
			}
		}
	}

	if(isset($this->parsed_blocks[$bname]))
	{
		$this->parsed_blocks[$bname].=$copy;
	}
	else
	{
		$this->parsed_blocks[$bname]=$copy;
	}

	// reset sub-blocks
	if ($this->AUTORESET && (!empty($this->sub_blocks[$bname]))) {
		reset($this->sub_blocks[$bname]);
		foreach ($this->sub_blocks[$bname] as $v)
			$this->reset($v);
	}
}

/***[ exists ]**************************************************************/
/*
	returns true if a block exists otherwise returns false.
*/
public function exists($bname){
	return (!empty($this->parsed_blocks[$bname])) || (!empty($this->blocks[$bname]));
}


/***[ var_exists ]**************************************************************/
/*
	returns true if a block exists otherwise returns false.
*/
public function var_exists($bname,$vname){
	if(!empty($this->blocks[$bname])){
		return substr_count((string) $this->blocks[$bname], '{'. $vname . '}') >0;
	}
	return false;
}


/***[ rparse ]**************************************************************/
/*
	returns the parsed text for a block, including all sub-blocks.
*/

public function rparse($bname) {
	if (!empty($this->sub_blocks[$bname])) {
		reset($this->sub_blocks[$bname]);
		foreach($this->sub_blocks[$bname] as $k => $v) {
            if (!empty($v)) {
                $this->rparse($v, $indent . "\t");
            }
		}

	}
	$this->parse($bname);
}

/***[ insert_loop ]*********************************************************/
/*
	inserts a loop ( call assign & parse )
*/

public function insert_loop($bname,$var,$value="") {
	$this->assign($var,$value);
	$this->parse($bname);
}

/***[ text ]****************************************************************/
/*
	returns the parsed text for a block
*/

public function text($bname) {

    if(!empty($this->parsed_blocks)){
	   return $this->parsed_blocks[isset($bname) ? $bname :$this->mainblock];
    }else{
        return '';
    }
}

/***[ out ]*****************************************************************/
/*
	prints the parsed text
*/

public function out ($bname) {
	global $focus;

	if(isset($focus)){
		global $action;

		if($focus && is_subclass_of($focus, 'SugarBean') && !$focus->ACLAccess($action)){

			ACLController::displayNoAccess(true);

			sugar_die('');
			return;
	}}

	echo $this->text($bname);
}

/***[ reset ]***************************************************************/
/*
	resets the parsed text
*/

public function reset ($bname) {
	$this->parsed_blocks[$bname]="";
}

/***[ parsed ]**************************************************************/
/*
	returns true if block was parsed, false if not
*/

public function parsed ($bname) {
	return (!empty($this->parsed_blocks[$bname]));
}

/***[ SetNullString ]*******************************************************/
/*
	sets the string to replace in case the var was not assigned
*/

public function SetNullString($str,$varname="") {
	$this->NULL_STRING[$varname]=$str;
}

/***[ SetNullBlock ]********************************************************/
/*
	sets the string to replace in case the block was not parsed
*/

public function SetNullBlock($str,$bname="") {
	$this->NULL_BLOCK[$bname]=$str;
}

/***[ set_autoreset ]*******************************************************/
/*
	sets AUTORESET to 1. (default is 1)
	if set to 1, parse() automatically resets the parsed blocks' sub blocks
	(for multiple level blocks)
*/

public function set_autoreset() {
	$this->AUTORESET=1;
}

/***[ clear_autoreset ]*****************************************************/
/*
	sets AUTORESET to 0. (default is 1)
	if set to 1, parse() automatically resets the parsed blocks' sub blocks
	(for multiple level blocks)
*/

public function clear_autoreset() {
	$this->AUTORESET=0;
}

/***[ scan_globals ]********************************************************/
/*
	scans global variables
*/

public function scan_globals() {
	$GLOB = [];
    reset($GLOBALS);
	foreach ($GLOBALS as $k => $v) {
        $GLOB[$k] = $v;
	}

	$this->assign("PHP",$GLOB);	/* access global variables as {PHP.HTTP_HOST} in your template! */
}

/******

		WARNING
		PUBLIC FUNCTIONS BELOW THIS LINE DIDN'T GET TESTED

******/


/***************************************************************************/
/***[ private stuff ]*******************************************************/
/***************************************************************************/

/***[ maketree ]************************************************************/
/*
	generates the array containing to-be-parsed stuff:
  $blocks["main"],$blocks["main.table"],$blocks["main.table.row"], etc.
	also builds the reverse parse order.
*/


public function maketree($con,$block) {
	$con2=explode($this->block_start_delim,$con);
	$level=0;
	$block_names=array();
	$blocks=array();
	reset($con2);
	foreach ($con2 as $k => $v) {
		$patt="($this->block_start_word|$this->block_end_word)\s*(\w+)\s*$this->block_end_delim(.*)";
		if (preg_match_all("/$patt/ims",$v,$res, PREG_SET_ORDER)) {
			// $res[0][1] = BEGIN or END
			// $res[0][2] = block name
			// $res[0][3] = kinda content
			if ($res[0][1]==$this->block_start_word) {
				$parent_name=implode(".",$block_names);
				$block_names[++$level]=$res[0][2];							/* add one level - array("main","table","row")*/
				$cur_block_name=implode(".",$block_names);	/* make block name (main.table.row) */
				$this->block_parse_order[]=$cur_block_name;	/* build block parsing order (reverse) */

				if(array_key_exists($cur_block_name, $blocks))
				{
					$blocks[$cur_block_name].=$res[0][3];				/* add contents */
				}
				else
				{
					$blocks[$cur_block_name]=$res[0][3];				/* add contents */
				}

				/* add {_BLOCK_.blockname} string to parent block */
				if(array_key_exists($parent_name, $blocks))
				{
					$blocks[$parent_name].="{_BLOCK_.$cur_block_name}";
				}
				else
				{
					$blocks[$parent_name]="{_BLOCK_.$cur_block_name}";
				}

				$this->sub_blocks[$parent_name][]=$cur_block_name;		/* store sub block names for autoresetting and recursive parsing */
				$this->sub_blocks[$cur_block_name][]="";		/* store sub block names for autoresetting */
			} else if ($res[0][1]==$this->block_end_word) {
				unset($block_names[$level--]);
				$parent_name=implode(".",$block_names);
				$blocks[$parent_name].=$res[0][3];	/* add rest of block to parent block */
  			}
		} else { /* no block delimiters found */
			$index = implode(".",$block_names);
			if(array_key_exists($index, $blocks))
			{
				$blocks[].=$this->block_start_delim.$v;
			}
			else
			{
				$blocks[]=$this->block_start_delim.$v;
			}
		}
	}
	return $blocks;
}



/***[ error stuff ]*********************************************************/
/*
	sets and gets error
*/

public function get_error()	{
	return ($this->ERROR=="")?0:$this->ERROR;
}


public function set_error($str)	{
	$this->ERROR=$str;
}

/***[ getfile ]*************************************************************/
/*
	returns the contents of a file
*/

public function getfile($file) {
	if (!isset($file)) {
		$this->set_error("!isset file name!");
		return "";
	}

	// Pick which folder we should include from
	// Prefer the local directory, then try the theme directory.
	if (!is_file($file))
		$file = $this->alternate_include_directory.$file;

	if(is_file($file))
	{
		$file_text=file_get_contents($file);

	} else {
		$this->set_error("[$file] does not exist");
		$file_text="<b>__XTemplate fatal error: file [$file] does not exist__</b>";
	}

	return $file_text;
}

/***[ r_getfile ]***********************************************************/
/*
	recursively gets the content of a file with {FILE "filename.tpl"} directives
*/


public function r_getfile($file) {
	$text=$this->getfile($file);
	while (preg_match($this->file_delim,(string) $text,$res)) {
		$text2=$this->getfile($res[1]);
		$text=str_replace($res[0], $text2, (string) $text);
	}
	return $text;
}

} /* end of XTemplate class. */

?>
