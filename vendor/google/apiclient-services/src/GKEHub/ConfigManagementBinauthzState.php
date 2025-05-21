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

namespace Google\Service\GKEHub;

class ConfigManagementBinauthzState extends \Google\Model
{
  protected $versionType = ConfigManagementBinauthzVersion::class;
  protected $versionDataType = '';
  /**
   * @var string
   */
  public $webhook;

  /**
   * @param ConfigManagementBinauthzVersion
   */
  public function setVersion(ConfigManagementBinauthzVersion $version)
  {
    $this->version = $version;
  }
  /**
   * @return ConfigManagementBinauthzVersion
   */
  public function getVersion()
  {
    return $this->version;
  }
  /**
   * @param string
   */
  public function setWebhook($webhook)
  {
    $this->webhook = $webhook;
  }
  /**
   * @return string
   */
  public function getWebhook()
  {
    return $this->webhook;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(ConfigManagementBinauthzState::class, 'Google_Service_GKEHub_ConfigManagementBinauthzState');
