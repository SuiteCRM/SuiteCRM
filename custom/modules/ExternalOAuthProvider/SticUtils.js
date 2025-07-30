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


/* HEADER */
// Set module name
var module = "ExternalOAuthProvider";

/* INCLUDES */

/* VALIDATION DEPENDENCIES */

/* DIRECT VALIDATION CALLBACKS */

/* VIEWS CUSTOM CODE */
switch (viewType()) {
    case "edit":
    case "quickcreate":
    case "popup":
        // Get connector selected value and show de default config info
        connectorInput = document.getElementById('connector');
        connectorValue = connectorInput.options[connectorInput.selectedIndex].value;
        showDefaultConfigConnector('edit', connectorValue);

        // Add change event listener
        connectorInput.addEventListener("change", function () {
            connectorValue = connectorInput.options[connectorInput.selectedIndex].value;
            showDefaultConfigConnector('edit', connectorValue);
        });

    break;

    case "detail":
        // Get connector selected value and show de default config info
        connectorValue = document.getElementById('connector_span').textContent.trim();
        showDefaultConfigConnector('detail', connectorValue);
    break;
}


/**
 * Configure default values ​​depending on the selected connector and display information depending of view type
 */
function showDefaultConfigConnector(view, connectorValue) 
{
    switch (connectorValue) {
        case "Stic Google":
            sticGoogleDefaultConfig = {
                scope: [
                    "https://mail.google.com/"
                ],
                urlOptions: {
                    urlAuthorize:"https://accounts.google.com/o/oauth2/auth",
                    urlAccessToken:"https://oauth2.googleapis.com/token",
                },
                authorizeUrlOptions: {
                    approval_prompt:"force",
                    access_type:"offline",
                },
                extraProviderParams:{
                    urlResourceOwnerDetails:'',
                }
            };
            displayDefaultConfigOfConnector(view, sticGoogleDefaultConfig);
        break;

        case "Stic Microsoft":
            sticMicrosoftDefaultConfig = {
                scope: [
                    "https://outlook.office.com/IMAP.AccessAsUser.All",
                    "offline_access",
                    "User.Read"
                ],
                extraProviderParams:{
                    urlResourceOwnerDetails:'',
                }
            };
            displayDefaultConfigOfConnector(view, sticMicrosoftDefaultConfig);
        break;

        case "Microsoft":
            microsoftDefaultConfig = {
                extraProviderParams:{
                    urlResourceOwnerDetails:'',
                }
            };
            displayDefaultConfigOfConnector(view, microsoftDefaultConfig);
        break;

        case "Generic":
            genericDefaultConfig = null;
            displayDefaultConfigOfConnector(view, genericDefaultConfig);
        break;
    }
}


/**
 * Displays the default configuration of the selected connector depending on the view the user is in.
 */
function displayDefaultConfigOfConnector(view, data)
{
    // Remove defaultConfigData Div Element if exists
    defaultConfigDataContainer = document.getElementById('defaultConfigData');
    if (defaultConfigDataContainer) {
        defaultConfigDataContainer.remove();
    }
    if (data) 
    {
        // Create default Config Data Container
        defaultConfigDataContainer = document.createElement('div');
        defaultConfigDataContainer.id = 'defaultConfigData';

        if (view == 'edit') {
            defaultConfigDataContainer.style.margin = '0% 3.5% 1.5% 0%';   
        } else {
            defaultConfigDataContainer.style.margin = '0.5% 0.2% 1.5% 0%';   
        }
        
        // Create config Container
        configContainer = document.createElement('div');
        configContainer.className = 'config-container';

        // Create Rows Container
        addHeaderRow(configContainer, SUGAR.language.get('ExternalOAuthProvider', 'LBL_CONNECTOR_DEFAULT_CONFIGURED_OPTIONS'), 'h4');

        Object.entries(data).forEach(([key, value]) => 
        {
            switch (key) 
            {
                case 'scope':
                    field = SUGAR.language.get('ExternalOAuthProvider', 'LBL_SCOPE');
                    addHeaderRow(configContainer, field);
                    value.forEach( elem => {
                        addKeyValueRow(configContainer, '', elem);
                    });
                    break;
                    
                case 'urlOptions':
                case 'authorizeUrlOptions':
                case 'extraProviderParams':
                    if (key == 'authorizeUrlOptions') {
                        field = SUGAR.language.get('ExternalOAuthProvider', 'LBL_AUTHORIZE_URL_OPTIONS');;
                    } else {
                        field = SUGAR.language.get('ExternalOAuthProvider', 'LBL_EXTRA_PROVIDER_PARAMS');;
                    }
                    addHeaderRow(configContainer, field, 'div');
                    Object.entries(value).forEach(([k, v]) => 
                    {
                        option = k + ': '
                        addKeyValueRow(configContainer, option, v);
                    });
                    break;
            }
        });
        defaultConfigDataContainer.appendChild(configContainer);

        // Select the row element that will remain below the div to be added
        if (view == 'edit') {
            row = $('#connector_span').closest('.edit-view-row-item').nextAll('.clear').first();
        } else {
            row = $('div.detail-view-row-item[data-field="connector"]').closest('.detail-view-row').next();
        }

        // Insert clearDiv and defaultConfigDataContainer before the first "clear" after the Connector field
        $(defaultConfigDataContainer).insertBefore(row);
        
        // Create and Insert "clear" Div Element
        const clearDiv = document.createElement('div');
        clearDiv.className = 'clear';
        $(clearDiv).insertBefore(defaultConfigDataContainer);

    }
}

/**
 * Add a header row 
 */
function addHeaderRow(configContainer, label= '', element)
{
    const configRow = document.createElement('div');
    configRow.className = 'config-row';

    if (label) {
        const fullCell = document.createElement(element);
        fullCell.className = 'config-cell-full';
        fullCell.textContent = label;
        configRow.appendChild(fullCell);
    }
    configContainer.appendChild(configRow);
}

/**
 * Add a key-value row 
 */
function addKeyValueRow(configContainer, key, value = '')
{
    const configRow = document.createElement('div');
    configRow.className = 'config-row';

    if (key) {
        const leftCell = document.createElement('div');
        leftCell.className = 'config-cell-left';
        leftCell.textContent = key;
        configRow.appendChild(leftCell);
    }

    const rightCell = document.createElement('div');
    rightCell.textContent = value;
    rightCell.className = 'config-cell-right';
    configRow.appendChild(rightCell);

    configContainer.appendChild(configRow);
}

/**
 * Add CSS Styles
 */
function addCSS() {
    const css = `
        #defaultConfigData {
            display: flex;
            justify-content: flex-end;
        }
        
        .config-container {
            display: flex;
            flex-direction: column;
            background-color: rgb(245, 245, 245);      
            min-width: 48%;
        }
        
        .config-row {
            display: flex;
            min-height: auto;
        }
        
        .config-cell-full {
            font-weight: bold;
            padding: 1rem;
        }

        .config-cell-left {
            width: 40%;
            padding: 0 1rem;
        }
        
        .config-cell-right {
            width: 60%;
            padding: 0.2rem 1rem;
            word-break: break-all;
            overflow-wrap: break-word;
        }
    `;

    const style = document.createElement('style');
    style.innerHTML = css;
    document.head.appendChild(style);
}

// Llamar a la función para añadir el CSS
addCSS();