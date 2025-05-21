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

class GoogleAppsCloudidentityDevicesV1AndroidAttributes extends \Google\Model
{
  /**
   * @var bool
   */
  public $ctsProfileMatch;
  /**
   * @var bool
   */
  public $enabledUnknownSources;
  /**
   * @var bool
   */
  public $hasPotentiallyHarmfulApps;
  /**
   * @var bool
   */
  public $ownerProfileAccount;
  /**
   * @var string
   */
  public $ownershipPrivilege;
  /**
   * @var bool
   */
  public $supportsWorkProfile;
  /**
   * @var bool
   */
  public $verifiedBoot;
  /**
   * @var bool
   */
  public $verifyAppsEnabled;

  /**
   * @param bool
   */
  public function setCtsProfileMatch($ctsProfileMatch)
  {
    $this->ctsProfileMatch = $ctsProfileMatch;
  }
  /**
   * @return bool
   */
  public function getCtsProfileMatch()
  {
    return $this->ctsProfileMatch;
  }
  /**
   * @param bool
   */
  public function setEnabledUnknownSources($enabledUnknownSources)
  {
    $this->enabledUnknownSources = $enabledUnknownSources;
  }
  /**
   * @return bool
   */
  public function getEnabledUnknownSources()
  {
    return $this->enabledUnknownSources;
  }
  /**
   * @param bool
   */
  public function setHasPotentiallyHarmfulApps($hasPotentiallyHarmfulApps)
  {
    $this->hasPotentiallyHarmfulApps = $hasPotentiallyHarmfulApps;
  }
  /**
   * @return bool
   */
  public function getHasPotentiallyHarmfulApps()
  {
    return $this->hasPotentiallyHarmfulApps;
  }
  /**
   * @param bool
   */
  public function setOwnerProfileAccount($ownerProfileAccount)
  {
    $this->ownerProfileAccount = $ownerProfileAccount;
  }
  /**
   * @return bool
   */
  public function getOwnerProfileAccount()
  {
    return $this->ownerProfileAccount;
  }
  /**
   * @param string
   */
  public function setOwnershipPrivilege($ownershipPrivilege)
  {
    $this->ownershipPrivilege = $ownershipPrivilege;
  }
  /**
   * @return string
   */
  public function getOwnershipPrivilege()
  {
    return $this->ownershipPrivilege;
  }
  /**
   * @param bool
   */
  public function setSupportsWorkProfile($supportsWorkProfile)
  {
    $this->supportsWorkProfile = $supportsWorkProfile;
  }
  /**
   * @return bool
   */
  public function getSupportsWorkProfile()
  {
    return $this->supportsWorkProfile;
  }
  /**
   * @param bool
   */
  public function setVerifiedBoot($verifiedBoot)
  {
    $this->verifiedBoot = $verifiedBoot;
  }
  /**
   * @return bool
   */
  public function getVerifiedBoot()
  {
    return $this->verifiedBoot;
  }
  /**
   * @param bool
   */
  public function setVerifyAppsEnabled($verifyAppsEnabled)
  {
    $this->verifyAppsEnabled = $verifyAppsEnabled;
  }
  /**
   * @return bool
   */
  public function getVerifyAppsEnabled()
  {
    return $this->verifyAppsEnabled;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GoogleAppsCloudidentityDevicesV1AndroidAttributes::class, 'Google_Service_CloudIdentity_GoogleAppsCloudidentityDevicesV1AndroidAttributes');
