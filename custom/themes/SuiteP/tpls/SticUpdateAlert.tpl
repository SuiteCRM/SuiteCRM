{*
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
 *}
{if $showUpdateAlert == 1}
    <div id="stic-notice" class="content hidden">
        <div class="alert alert-danger alert-dismissible" role="alert">
 
            <a title="{$APP.LBL_STIC_UPDATE_ALERT_CLOSE}" id="close-stic-notice" type="button" class="pull-right btn btn-xs btn-info"
                data-dismiss="alert" aria-label="Close">{$APP.LBL_HIDE} <span class="glyphicon glyphicon-remove" aria-hidden="true"></span></a>
            <p>
                <strong>{$APP.LBL_STIC_UPDATE_ALERT_TITLE} [v {$lastSticVersion}]</strong>
            </p>


            <p>{$APP.LBL_STIC_UPDATE_ALERT_INFO}</p>

            <p>
                <a class="btn btn-default btn-sm" href="https://forums.sinergiacrm.org/viewforum.php?f=17" target="_blank"
                    title="v.{$lastSticVersion} ">
                    <i class="glyphicon glyphicon-link" data-hasqtip="3"></i> {$APP.LBL_STIC_UPDATE_ALERT_LINK}
                </a>
            </p>
        </div>
    </div>
    {literal}
        <script>
            // Set cookie on closing update alert 
            $('#close-stic-notice').on('click', function() {
                Set_Cookie('SticVersion', {/literal}'{$lastSticVersion}'{literal},10000 , '/', false, false);
                $('#stic-notice').remove()
            })
           
            if ($('#bootstrap-container #stic-notice').length == 0) {
                setTimeout(function() {
                    $('#stic-notice').prependTo('#bootstrap-container').removeClass('hidden')
                }, 1) 

            }
           
        </script>
    {/literal}
{/if}
