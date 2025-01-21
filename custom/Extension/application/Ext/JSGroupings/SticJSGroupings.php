<?php
/**
 * This file is part of SinergiaCRM.
 * SinergiaCRM is a work developed by SinergiaTIC Association, based on SuiteCRM.
 * Copyright (C) 2013 - 2023 SinergiaTIC Association
 *
 * This program is free software; you can redistribute it and/or modify it under
 * the terms of the GNU Affero General Public License version 3 as published by the
 * Free Software Foundation.
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
 * You can contact SinergiaTIC Association at email address info@sinergiacrm.org.
 */

// Add js file for some customizations in Popup views
$js_groupings[] = $newGrouping = array(
    'SticInclude/js/SticPopupCustomizations.js' => 'include/javascript/sugar_grp1.js',
);

// Adding selectize library to JSGroupings.
// JSGroupings are file packages containing one or more minified JavaScript libraries.
// The groupings enhance system performance by reducing the number of downloaded files for a given page.
// The resulting files will be created inside the /cache/ folder.
$js_groupings[] = $newGrouping = array(
    'SticInclude/vendor/selectize/js/selectize.min.js' => 'include/javascript/sugar_grp1.js',
    'SticInclude/js/SticSelectize.js' => 'include/javascript/sugar_grp1.js',
);

// Overrides isValidEmail function and fix STIC#301
$js_groupings[] = $newGrouping = array(
    'SticInclude/js/SticCustomIsValidEmailFunction.js' => 'include/javascript/sugar_grp1.js',
);

// Add autosize library & runit
$js_groupings[] = $newGrouping = array(
    'SticInclude/vendor/autosize/dist/autosize.min.js' => 'include/javascript/sugar_grp1.js',
    'SticInclude/js/SticAutosize.js' => 'include/javascript/sugar_grp1.js',
);

// Add qtip library in all pages and custom qtip calls
$js_groupings[] = $newGrouping = array(
    'include/javascript/qtip/jquery.qtip.min.js' => 'include/javascript/sugar_grp1.js',
    'SticInclude/js/SticQtip.js' => 'include/javascript/sugar_grp1.js',
    
);
// Add qtip library in all pages and custom qtip calls
$js_groupings[] = $newGrouping = array(
    'SticInclude/js/SticGetAdditionalDetails.js' => 'include/javascript/sugar_grp1.js',
);

// Adding moment library to JSGroupings.
$js_groupings[] = $newGrouping = array(
    'include/javascript/moment.min.js' => 'include/javascript/sugar_grp1.js',
);

// Add Custom View functionality
$js_groupings[] = $newGrouping = array(
    'modules/stic_Custom_Views/processor/js/sticCVUtils.js' => 'include/javascript/sugar_grp1.js',

    'modules/stic_Custom_Views/processor/js/sticCV_Element_Div.js' => 'include/javascript/sugar_grp1.js',
    'modules/stic_Custom_Views/processor/js/sticCV_Element_Label.js' => 'include/javascript/sugar_grp1.js',

    'modules/stic_Custom_Views/processor/js/sticCV_Record_Container.js' => 'include/javascript/sugar_grp1.js',

    'modules/stic_Custom_Views/processor/js/sticCV_Record_Field_Container.js' => 'include/javascript/sugar_grp1.js',
    'modules/stic_Custom_Views/processor/js/sticCV_Record_Field_Header.js' => 'include/javascript/sugar_grp1.js',
    'modules/stic_Custom_Views/processor/js/sticCV_Record_Field_Content.js' => 'include/javascript/sugar_grp1.js',
    'modules/stic_Custom_Views/processor/js/sticCV_Record_Field.js' => 'include/javascript/sugar_grp1.js',

    'modules/stic_Custom_Views/processor/js/sticCV_Element_FieldContainer.js' => 'include/javascript/sugar_grp1.js',

    'modules/stic_Custom_Views/processor/js/sticCV_Record_Tab_Header.js' => 'include/javascript/sugar_grp1.js',
    'modules/stic_Custom_Views/processor/js/sticCV_Record_Tab_Content.js' => 'include/javascript/sugar_grp1.js',
    'modules/stic_Custom_Views/processor/js/sticCV_Record_Tab.js' => 'include/javascript/sugar_grp1.js',

    'modules/stic_Custom_Views/processor/js/sticCV_Record_Panel_Container.js' => 'include/javascript/sugar_grp1.js',
    'modules/stic_Custom_Views/processor/js/sticCV_Record_Panel_Header.js' => 'include/javascript/sugar_grp1.js',
    'modules/stic_Custom_Views/processor/js/sticCV_Record_Panel_Content.js' => 'include/javascript/sugar_grp1.js',
    'modules/stic_Custom_Views/processor/js/sticCV_Record_Panel.js' => 'include/javascript/sugar_grp1.js',

    'modules/stic_Custom_Views/processor/js/sticCV_View_Record_Base.js' => 'include/javascript/sugar_grp1.js',
    'modules/stic_Custom_Views/processor/js/sticCV_View_Record_Detail.js' => 'include/javascript/sugar_grp1.js',
    'modules/stic_Custom_Views/processor/js/sticCV_View_Record_Edit.js' => 'include/javascript/sugar_grp1.js',

    'modules/stic_Custom_Views/processor/js/sticCustomizeView.js' => 'include/javascript/sugar_grp1.js',
);


// Add Advanced multilevel menu  functionality
$js_groupings[] = $newGrouping = array(
    'SticInclude/vendor/smartmenus/dist/jquery.smartmenus.min.js' => 'include/javascript/sugar_grp1.js',
    'SticInclude/js/SticSmartmenus.js' => 'include/javascript/sugar_grp1.js',
);
