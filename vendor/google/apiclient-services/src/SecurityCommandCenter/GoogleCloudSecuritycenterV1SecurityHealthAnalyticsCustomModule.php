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

namespace Google\Service\SecurityCommandCenter;

class GoogleCloudSecuritycenterV1SecurityHealthAnalyticsCustomModule extends \Google\Model
{
  /**
   * @var string
   */
  public $ancestorModule;
  /**
   * @var string
   */
  public $cloudProvider;
  protected $customConfigType = GoogleCloudSecuritycenterV1CustomConfig::class;
  protected $customConfigDataType = '';
  /**
   * @var string
   */
  public $displayName;
  /**
   * @var string
   */
  public $enablementState;
  /**
   * @var string
   */
  public $lastEditor;
  /**
   * @var string
   */
  public $name;
  /**
   * @var string
   */
  public $updateTime;

  /**
   * @param string
   */
  public function setAncestorModule($ancestorModule)
  {
    $this->ancestorModule = $ancestorModule;
  }
  /**
   * @return string
   */
  public function getAncestorModule()
  {
    return $this->ancestorModule;
  }
  /**
   * @param string
   */
  public function setCloudProvider($cloudProvider)
  {
    $this->cloudProvider = $cloudProvider;
  }
  /**
   * @return string
   */
  public function getCloudProvider()
  {
    return $this->cloudProvider;
  }
  /**
   * @param GoogleCloudSecuritycenterV1CustomConfig
   */
  public function setCustomConfig(GoogleCloudSecuritycenterV1CustomConfig $customConfig)
  {
    $this->customConfig = $customConfig;
  }
  /**
   * @return GoogleCloudSecuritycenterV1CustomConfig
   */
  public function getCustomConfig()
  {
    return $this->customConfig;
  }
  /**
   * @param string
   */
  public function setDisplayName($displayName)
  {
    $this->displayName = $displayName;
  }
  /**
   * @return string
   */
  public function getDisplayName()
  {
    return $this->displayName;
  }
  /**
   * @param string
   */
  public function setEnablementState($enablementState)
  {
    $this->enablementState = $enablementState;
  }
  /**
   * @return string
   */
  public function getEnablementState()
  {
    return $this->enablementState;
  }
  /**
   * @param string
   */
  public function setLastEditor($lastEditor)
  {
    $this->lastEditor = $lastEditor;
  }
  /**
   * @return string
   */
  public function getLastEditor()
  {
    return $this->lastEditor;
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
  /**
   * @param string
   */
  public function setUpdateTime($updateTime)
  {
    $this->updateTime = $updateTime;
  }
  /**
   * @return string
   */
  public function getUpdateTime()
  {
    return $this->updateTime;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GoogleCloudSecuritycenterV1SecurityHealthAnalyticsCustomModule::class, 'Google_Service_SecurityCommandCenter_GoogleCloudSecuritycenterV1SecurityHealthAnalyticsCustomModule');
