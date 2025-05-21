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

namespace Google\Service\CloudIdentity;

class SamlIdpConfig extends \Google\Model
{
  /**
   * @var string
   */
  public $changePasswordUri;
  /**
   * @var string
   */
  public $entityId;
  /**
   * @var string
   */
  public $logoutRedirectUri;
  /**
   * @var string
   */
  public $singleSignOnServiceUri;

  /**
   * @param string
   */
  public function setChangePasswordUri($changePasswordUri)
  {
    $this->changePasswordUri = $changePasswordUri;
  }
  /**
   * @return string
   */
  public function getChangePasswordUri()
  {
    return $this->changePasswordUri;
  }
  /**
   * @param string
   */
  public function setEntityId($entityId)
  {
    $this->entityId = $entityId;
  }
  /**
   * @return string
   */
  public function getEntityId()
  {
    return $this->entityId;
  }
  /**
   * @param string
   */
  public function setLogoutRedirectUri($logoutRedirectUri)
  {
    $this->logoutRedirectUri = $logoutRedirectUri;
  }
  /**
   * @return string
   */
  public function getLogoutRedirectUri()
  {
    return $this->logoutRedirectUri;
  }
  /**
   * @param string
   */
  public function setSingleSignOnServiceUri($singleSignOnServiceUri)
  {
    $this->singleSignOnServiceUri = $singleSignOnServiceUri;
  }
  /**
   * @return string
   */
  public function getSingleSignOnServiceUri()
  {
    return $this->singleSignOnServiceUri;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(SamlIdpConfig::class, 'Google_Service_CloudIdentity_SamlIdpConfig');
