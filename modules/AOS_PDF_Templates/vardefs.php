<?php
/**
 *
 * SugarCRM Community Edition is a customer relationship management program developed by
 * SugarCRM, Inc. Copyright (C) 2004-2013 SugarCRM Inc.
 *
 * SuiteCRM is an extension to SugarCRM Community Edition developed by SalesAgility Ltd.
 * Copyright (C) 2011 - 2018 SalesAgility Ltd.
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

$dictionary['AOS_PDF_Templates'] = array(
    'table'=>'aos_pdf_templates',
    'audited'=>true,
    'fields'=>array(
  'active' =>
  array(
    'name' => 'active',
    'vname' => 'LBL_ACTIVE',
    'type' => 'bool',
    'massupdate' => 0,
    'default' => true,
    'comments' => '',
    'help' => '',
    'importable' => 'true',
    'duplicate_merge' => 'disabled',
    'duplicate_merge_dom_value' => '0',
    'audited' => 1,
    'reportable' => 0,
    'studio' => 'visible',
  ),
  'type' =>
  array(
    'required' => '1',
    'name' => 'type',
    'vname' => 'LBL_TYPE',
    'type' => 'enum',
    'massupdate' => 0,
    'default' => '',
    'comments' => '',
    'help' => '',
    'importable' => 'true',
    'duplicate_merge' => 'disabled',
    'duplicate_merge_dom_value' => '0',
    'audited' => 1,
    'reportable' => 0,
    'len' => 100,
    'options' => 'pdf_template_type_dom',
    'studio' => 'visible',
  ),
        'description' =>
            array(
                'name' => 'description',
                'vname' => 'LBL_DESCRIPTION',
                'type' => 'longtext',
                'comment' => 'Full text of the note',
                'rows' => '6',
                'cols' => '80',
                'required' => false,
                'massupdate' => 0,
                'no_default' => false,
                'comments' => 'Full text of the note',
                'help' => '',
                'importable' => 'true',
                'duplicate_merge' => 'disabled',
                'duplicate_merge_dom_value' => '0',
                'audited' => false,
                'reportable' => true,
                'unified_search' => false,
                'merge_filter' => 'disabled',
                'size' => '20',
                'studio' => 'visible',
            ),
  'sample' =>
  array(
    'required' => '0',
    'name' => 'sample',
    'vname' => 'LBL_SAMPLE',
    'source' => 'non-db',
    'type' => 'enum',
    'massupdate' => 0,
    'default' => '',
    'comments' => '',
    'help' => '',
    'importable' => 'true',
    'duplicate_merge' => 'disabled',
    'duplicate_merge_dom_value' => '0',
    'audited' => 1,
    'reportable' => 0,
    'len' => 100,
    'options' => 'pdf_template_sample_dom',
    'studio' => 'visible',
  ),
  'insert_fields' =>
  array(
      'required' => '0',
      'name' => 'insert_fields',
      'vname' => 'LBL_INSERT_FIELDS',
      'studio' => 'visible',
      'source' => 'non-db',
      'type' => 'enum',
      'massupdate' => 0,
      'default' => '',
      'comments' => '',
      'help' => '',
      'reportable' => 0,
  ),
  'pdfheader' =>
  array(
    'required' => false,
    'name' => 'pdfheader',
    'vname' => 'LBL_HEADER',
    'type' => 'longtext',
    'massupdate' => 0,
    'comments' => '',
    'help' => '',
    'importable' => 'true',
    'duplicate_merge' => 'disabled',
    'duplicate_merge_dom_value' => '0',
    'audited' => false,
    'reportable' => true,
    'size' => '20',
    'studio' => 'visible',
    'rows' => '4',
    'cols' => '20',
  ),
  'pdffooter' =>
  array(
    'required' => false,
    'name' => 'pdffooter',
    'vname' => 'LBL_FOOTER',
    'type' => 'longtext',
    'massupdate' => 0,
    'comments' => '',
    'help' => '',
    'importable' => 'true',
    'duplicate_merge' => 'disabled',
    'duplicate_merge_dom_value' => '0',
    'audited' => false,
    'reportable' => true,
    'size' => '20',
    'studio' => 'visible',
    'rows' => '4',
    'cols' => '20',
  ),
  'margin_left' =>
  array(
    'required' => false,
    'name' => 'margin_left',
    'vname' => 'LBL_MARGIN_LEFT',
    'type' => 'int',
    'massupdate' => 0,
    'default' => '15',
    'comments' => '',
    'help' => '',
    'importable' => 'true',
    'duplicate_merge' => 'disabled',
    'duplicate_merge_dom_value' => '0',
    'audited' => false,
    'reportable' => true,
    'len' => '255',
    'size' => '20',
    'enable_range_search' => false,
    'disable_num_format' => '',
  ),
  'margin_right' =>
  array(
    'required' => false,
    'name' => 'margin_right',
    'vname' => 'LBL_MARGIN_RIGHT',
    'type' => 'int',
    'massupdate' => 0,
    'default' => '15',
    'comments' => '',
    'help' => '',
    'importable' => 'true',
    'duplicate_merge' => 'disabled',
    'duplicate_merge_dom_value' => '0',
    'audited' => false,
    'reportable' => true,
    'len' => '255',
    'size' => '20',
    'enable_range_search' => false,
    'disable_num_format' => '',
  ),
  'margin_top' =>
  array(
    'required' => false,
    'name' => 'margin_top',
    'vname' => 'LBL_MARGIN_TOP',
    'type' => 'int',
    'massupdate' => 0,
    'default' => '16',
    'comments' => '',
    'help' => '',
    'importable' => 'true',
    'duplicate_merge' => 'disabled',
    'duplicate_merge_dom_value' => '0',
    'audited' => false,
    'reportable' => true,
    'len' => '255',
    'size' => '20',
    'enable_range_search' => false,
    'disable_num_format' => '',
  ),
  'margin_bottom' =>
  array(
    'required' => false,
    'name' => 'margin_bottom',
    'vname' => 'LBL_MARGIN_BOTTOM',
    'type' => 'int',
    'massupdate' => 0,
    'default' => '16',
    'comments' => '',
    'help' => '',
    'importable' => 'true',
    'duplicate_merge' => 'disabled',
    'duplicate_merge_dom_value' => '0',
    'audited' => false,
    'reportable' => true,
    'len' => '255',
    'size' => '20',
    'enable_range_search' => false,
    'disable_num_format' => '',
  ),
  'margin_header' =>
  array(
    'required' => false,
    'name' => 'margin_header',
    'vname' => 'LBL_MARGIN_HEADER',
    'type' => 'int',
    'massupdate' => 0,
    'default' => '9',
    'comments' => '',
    'help' => '',
    'importable' => 'true',
    'duplicate_merge' => 'disabled',
    'duplicate_merge_dom_value' => '0',
    'audited' => false,
    'reportable' => true,
    'len' => '255',
    'size' => '20',
    'enable_range_search' => false,
    'disable_num_format' => '',
  ),
  'margin_footer' =>
  array(
    'required' => false,
    'name' => 'margin_footer',
    'vname' => 'LBL_MARGIN_FOOTER',
    'type' => 'int',
    'massupdate' => 0,
    'default' => '9',
    'comments' => '',
    'help' => '',
    'importable' => 'true',
    'duplicate_merge' => 'disabled',
    'duplicate_merge_dom_value' => '0',
    'audited' => false,
    'reportable' => true,
    'len' => '255',
    'size' => '20',
    'enable_range_search' => false,
    'disable_num_format' => '',
  ),
          'page_size' =>
            array(
                'required' => '0',
                'name' => 'page_size',
                'vname' => 'LBL_PAGE_SIZE',
                'type' => 'enum',
                'massupdate' => 0,
                'default' => '',
                'comments' => '',
                'help' => '',
                'importable' => 'true',
                'duplicate_merge' => 'disabled',
                'duplicate_merge_dom_value' => '0',
                'audited' => 0,
                'reportable' => 0,
                'len' => 100,
                'options' => 'pdf_page_size_dom',
                'studio' => 'visible',
            ),
        'orientation' =>
            array(
                'required' => '0',
                'name' => 'orientation',
                'vname' => 'LBL_ORIENTATION',
                'type' => 'enum',
                'massupdate' => 0,
                'default' => '',
                'comments' => '',
                'help' => '',
                'importable' => 'true',
                'duplicate_merge' => 'disabled',
                'duplicate_merge_dom_value' => '0',
                'audited' => 0,
                'reportable' => 0,
                'len' => 100,
                'options' => 'pdf_orientation_dom',
                'studio' => 'visible',
            ),
),
    'relationships'=>array(
),
    'optimistic_locking'=>true,
);
require_once('include/SugarObjects/VardefManager.php');
VardefManager::createVardef('AOS_PDF_Templates', 'AOS_PDF_Templates', array('basic','assignable','security_groups'));
