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

namespace Google\Service\AccessContextManager;

class ScopedAccessSettings extends \Google\Model
{
  protected $activeSettingsType = AccessSettings::class;
  protected $activeSettingsDataType = '';
  protected $dryRunSettingsType = AccessSettings::class;
  protected $dryRunSettingsDataType = '';
  protected $scopeType = AccessScope::class;
  protected $scopeDataType = '';

  /**
   * @param AccessSettings
   */
  public function setActiveSettings(AccessSettings $activeSettings)
  {
    $this->activeSettings = $activeSettings;
  }
  /**
   * @return AccessSettings
   */
  public function getActiveSettings()
  {
    return $this->activeSettings;
  }
  /**
   * @param AccessSettings
   */
  public function setDryRunSettings(AccessSettings $dryRunSettings)
  {
    $this->dryRunSettings = $dryRunSettings;
  }
  /**
   * @return AccessSettings
   */
  public function getDryRunSettings()
  {
    return $this->dryRunSettings;
  }
  /**
   * @param AccessScope
   */
  public function setScope(AccessScope $scope)
  {
    $this->scope = $scope;
  }
  /**
   * @return AccessScope
   */
  public function getScope()
  {
    return $this->scope;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(ScopedAccessSettings::class, 'Google_Service_AccessContextManager_ScopedAccessSettings');
