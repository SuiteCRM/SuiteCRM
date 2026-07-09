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
// Prevents directly accessing this file from a web browser
if (!defined('sugarEntry') || !sugarEntry) {
    die('Not A Valid Entry Point');
}

include_once "modules/stic_AWF_Forms/actions/coreActions.php";

/**
 * CheckSessionAction
 *
 * Action that verifies that the current user has a valid session in the system.
 * 
 */
class CheckSessionAction extends HookActionDefinition implements IFrontendAction
{
    public function __construct()
    {
        $this->isActive = true;
        $this->category = 'security'; 
        $this->baseLabel = 'LBL_CHECK_SESSION_ACTION';
        $this->isUserSelectable = true;
        $this->order = -99;
    }

    /**
     * Defines the parameters that will be displayed in the wizard.
     */
    public function getParameters(): array
    {
        $paramPermissionsErrorMsg = new ActionParameterDefinition();
        $paramPermissionsErrorMsg->name = 'permissions_error_message';
        $paramPermissionsErrorMsg->text = $this->translate('PERMISSIONS_ERROR_MSG_TEXT'); 
        $paramPermissionsErrorMsg->type = ActionParameterType::VALUE;
        $paramPermissionsErrorMsg->dataType = ActionDataType::TEXT;
        $paramPermissionsErrorMsg->defaultValue = $this->translate('PERMISSIONS_ERROR_MSG_TEXT_DEFAULT');
        $paramPermissionsErrorMsg->required = true;
        
        return [$paramPermissionsErrorMsg];
    }

    /**
     * Executes the redirect logic.
     */
    public function execute(ExecutionContext $context, FormAction $actionConfig): ActionResult
    {
        // We do not check $current_user because always is an admin (ResponseHandler sets current_user to admin for the execution of the actions)
        // Instead, we check if there is $defaultAssignedUserId defined in ExecutionContext with the loged user before the change to admin

        $formBean = BeanFactory::getBean('stic_AWF_Forms', $context->formId);
        if (!$formBean || empty($formBean->id)) {
            $GLOBALS['log']->fatal('Line ' . __LINE__ . ': ' . __METHOD__ . ": Form not found with ID '{$context->formId}'. Aborting session check.");
            return new ActionResult(ResultStatus::ERROR, $actionConfig, "Form not found");
        }

        if(empty($context->visitorUserId)) {
            $GLOBALS['log']->fatal('Line ' . __LINE__ . ': ' . __METHOD__ . ": No user session found in context for form '{$formBean->name}' (ID: {$formBean->id}). Aborting session check.");
            return new ActionResult(ResultStatus::ERROR, $actionConfig, "No user session found");
        }
        
        $authenticated_user = BeanFactory::getBean('Users', $context->visitorUserId);
        if (!$authenticated_user) {
            $GLOBALS['log']->fatal('Line ' . __LINE__ . ': ' . __METHOD__ . ": Authenticated user not found in session for form '{$formBean->name}' (ID: {$formBean->id}). Aborting session check.");
            return new ActionResult(ResultStatus::ERROR, $actionConfig, "Authenticated user not found");
        }

        return new ActionResult(ResultStatus::OK, $actionConfig, "User validated: " . $authenticated_user->user_name);
    }

    /**
     * Returns the assets (CSS/JS) that should be injected into the frontend for this action.
     * @param array $params Action parameters (unresolved)
     * @param FormConfig|null $formConfig Form configuration (if applicable)
     * @param string $formId Form ID (if applicable)
     * @return array array Structure: ['script' => ['console.log("hi")'], 'css' => [], 'html' => []]
     */
    public function getFrontendAssets(array $params, ?FormConfig $formConfig = null, string $formId = ''): array {
        $permissionsErrorMsg = $this->translate('PERMISSIONS_ERROR_MSG_TEXT_DEFAULT'); 
        foreach($params as $p) {
            if ($p->name == 'permissions_error_message') $permissionsErrorMsg = $p->value;
        }
        $jsPermissionsErrorMsg = json_encode($permissionsErrorMsg);
        $jsCheckingMsg = json_encode($this->translate('CHECKING'));
        $jsDeniedTitle = json_encode($this->translate('DENIED_TITLE'));
        $jsActiveSessionTxt = json_encode($this->translate('ACTIVE_SESSION'));

        // URL for login with redirection
        $loginUrl = "index.php?module=Users&action=Login&login_module=stic_AWF_Forms&login_action=renderForm&login_record={$formId}";
        $jsLoginUrl = json_encode($loginUrl);

        $script = <<<JS
        document.addEventListener('DOMContentLoaded', function() {
            // Initial visual blocking
            const wrapper = document.querySelector('.awf-main-card');
            if (wrapper) {
                wrapper.style.visibility = 'hidden'; 
                const loader = document.createElement('div');
                loader.id = 'awf-session-loader';
                loader.innerHTML = '<div class="text-center p-5"><div class="spinner-border text-primary mb-2"></div><div>' + {$jsCheckingMsg} + '</div><div>';
                wrapper.parentNode.insertBefore(loader, wrapper);
            }

            // AJAX Request to validate session and permissions
            fetch('index.php?entryPoint=stic_AWF_checkSession&id={$formId}', {
                method: 'GET',
                credentials: 'include', // Important to include cookies for session validation through CORS
                headers: {
                    'Accept': 'application/json',
                    'X-Requested-With': 'XMLHttpRequest'
                }
            })
                .then(r => r.json())
                .then(data => {
                    const loader = document.getElementById('awf-session-loader');
                    if(loader) loader.remove();

                    if (data.is_logged) {
                        // Check permissions
                        if (data.permissions) { 
                            if (wrapper) {
                                wrapper.style.visibility = 'visible';
                                // Add user badge with the name of the logged-in user
                                if (data.user_name) {
                                    const userBadge = document.createElement('div');
                                    userBadge.className = 'awf-user-badge text-end text-muted small mb-3';
                                    userBadge.style.opacity = '0.7';
                                    userBadge.innerHTML = '<span class="badge bg-light text-dark border"><i class="fas fa-user-check text-success"></i> ' + {$jsActiveSessionTxt} + ': <strong>' + data.user_name + '</strong></span>';
                                    wrapper.prepend(userBadge);
                                }
                            }
                        }
                        else {
                            // User does not have permissions
                            if (wrapper) {
                                wrapper.style.visibility = 'hidden';
                                const errorDiv = document.createElement('div');
                                let finalMsg = {$jsPermissionsErrorMsg};
                                errorDiv.className = 'alert alert-warning m-5 text-center shadow';
                                errorDiv.innerHTML = '<h4>' + {$jsDeniedTitle} + '</h4><p>' + finalMsg + '</p>';
                                wrapper.parentNode.insertBefore(errorDiv, wrapper);
                            } else {
                                alert({$jsPermissionsErrorMsg}); // Fallback alert if we can't manipulate the DOM
                            }
                        }
                    } else {
                        window.location.href = {$jsLoginUrl}; // Redirect to login
                    }
                })
                .catch(e => {
                    console.error("Session check error", e); 

                    // If fails fetch and is not a JSON response, we assume the session is invalid (SuiteCRM has intercepted the call)
                    const loader = document.getElementById('awf-session-loader');
                    if(loader) loader.remove();

                    window.location.href = {$jsLoginUrl}; // Redirect to login
                });
        });
JS;
        return ['script' => [$script]];
    }
}