/* HEADER */
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

// Set module name
var module = "EmailTemplates";

/* VIEWS CUSTOM CODE */
switch (viewType()) {
    case "edit":
    case "quickcreate":
    case "popup":    
    
        $(document).ready(function() {
            function toggleHtmlVisibility() {
                var selectedValue = $("[name=type]");
                
                if (selectedValue.length === 2) {
                  selectedValue = $("[name=type]").eq(1).val();
                }
                else {
                  selectedValue = $("[name=type]").val();
                }
                if (selectedValue === 'sms' || selectedValue === 'whatsapp') {
                    // $('#myDiv').hide();
                    $("#toggle_textonly").prop("checked", true);
                    $("#toggle_textonly").attr("disabled", true);
                    $("#text_only").val(1);
                    toggle_text_only();
                    // When editor in pop-up, the tiny editor is not contained in div assumed by std funtion 
                    // and it is created aftwerwards, so we use an observer to monitor modifications on dom after document ready
                		$(".tox.tox-tinymce").hide();
                    const observer = new MutationObserver((mutationsList, observer) => {
                      $(".tox.tox-tinymce").hide();
                    });
                    observer.observe(document.body, { childList: true });
                } else {
                    // $('#myDiv').show();
                    $("#toggle_textonly").prop("checked", false);
                    $("#toggle_textonly").attr("disabled", false);
                    $("#text_only").val(0);
                    toggle_text_only();
                    $(".tox.tox-tinymce").show();
                    const observer = new MutationObserver((mutationsList, observer) => {
                      $(".tox.tox-tinymce").show();
                    });
                    observer.observe(document.body, { childList: true });
                }
                

            }

            // Call the function on page load
            toggleHtmlVisibility();

            // Call the function when the select value changes
            $("[name=type]").change(toggleHtmlVisibility);
        });
  
      break;
  
    case "detail":
      // Set autofill mark beside field label
      if ($('#text_only').prop('checked')) {
        toggle_textarea(this);
        $("#html_div").hide();
      }
      break;
  
    case "list":
      break;
    default:
      break;
  }
  

