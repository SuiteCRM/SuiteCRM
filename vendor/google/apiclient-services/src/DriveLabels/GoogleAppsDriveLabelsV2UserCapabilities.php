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

namespace Google\Service\DriveLabels;

class GoogleAppsDriveLabelsV2UserCapabilities extends \Google\Model
{
  /**
   * @var bool
   */
  public $canAccessLabelManager;
  /**
   * @var bool
   */
  public $canAdministrateLabels;
  /**
   * @var bool
   */
  public $canCreateAdminLabels;
  /**
   * @var bool
   */
  public $canCreateSharedLabels;
  /**
   * @var string
   */
  public $name;

  /**
   * @param bool
   */
  public function setCanAccessLabelManager($canAccessLabelManager)
  {
    $this->canAccessLabelManager = $canAccessLabelManager;
  }
  /**
   * @return bool
   */
  public function getCanAccessLabelManager()
  {
    return $this->canAccessLabelManager;
  }
  /**
   * @param bool
   */
  public function setCanAdministrateLabels($canAdministrateLabels)
  {
    $this->canAdministrateLabels = $canAdministrateLabels;
  }
  /**
   * @return bool
   */
  public function getCanAdministrateLabels()
  {
    return $this->canAdministrateLabels;
  }
  /**
   * @param bool
   */
  public function setCanCreateAdminLabels($canCreateAdminLabels)
  {
    $this->canCreateAdminLabels = $canCreateAdminLabels;
  }
  /**
   * @return bool
   */
  public function getCanCreateAdminLabels()
  {
    return $this->canCreateAdminLabels;
  }
  /**
   * @param bool
   */
  public function setCanCreateSharedLabels($canCreateSharedLabels)
  {
    $this->canCreateSharedLabels = $canCreateSharedLabels;
  }
  /**
   * @return bool
   */
  public function getCanCreateSharedLabels()
  {
    return $this->canCreateSharedLabels;
  }
  /**
   * @param string
   */
  public function setName($name)
  {
    $this->name = $name;
  }
  /**
   * @return string
   */
  public function getName()
  {
    return $this->name;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GoogleAppsDriveLabelsV2UserCapabilities::class, 'Google_Service_DriveLabels_GoogleAppsDriveLabelsV2UserCapabilities');
