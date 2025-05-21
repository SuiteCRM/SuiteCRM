<?php
/*
 * Copyright 2014 Google Inc.
 *
 * Licensed under the Apache License, Version 2.0 (the "License"); you may not
 * use this file except in compliance with the License. You may obtain a copy of
 * the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
 * WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
 * License for the specific language governing permissions and limitations under
 * the License.
 */

namespace Google\Service\GoogleAnalyticsAdmin;

class GoogleAnalyticsAdminV1betaProvisionAccountTicketRequest extends \Google\Model
{
  protected $accountType = GoogleAnalyticsAdminV1betaAccount::class;
  protected $accountDataType = '';
  /**
   * @var string
   */
  public $redirectUri;

  /**
   * @param GoogleAnalyticsAdminV1betaAccount
   */
  public function setAccount(GoogleAnalyticsAdminV1betaAccount $account)
  {
    $this->account = $account;
  }
  /**
   * @return GoogleAnalyticsAdminV1betaAccount
   */
  public function getAccount()
  {
    return $this->account;
  }
  /**
   * @param string
   */
  public function setRedirectUri($redirectUri)
  {
    $this->redirectUri = $redirectUri;
  }
  /**
   * @return string
   */
  public function getRedirectUri()
  {
    return $this->redirectUri;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GoogleAnalyticsAdminV1betaProvisionAccountTicketRequest::class, 'Google_Service_GoogleAnalyticsAdmin_GoogleAnalyticsAdminV1betaProvisionAccountTicketRequest');
