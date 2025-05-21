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

namespace Google\Service\DisplayVideo;

class FloodlightActivity extends \Google\Collection
{
  protected $collection_key = 'remarketingConfigs';
  /**
   * @var string[]
   */
  public $advertiserIds;
  /**
   * @var string
   */
  public $displayName;
  /**
   * @var string
   */
  public $floodlightActivityId;
  /**
   * @var string
   */
  public $floodlightGroupId;
  /**
   * @var string
   */
  public $name;
  protected $remarketingConfigsType = RemarketingConfig::class;
  protected $remarketingConfigsDataType = 'array';
  /**
   * @var string
   */
  public $servingStatus;
  /**
   * @var bool
   */
  public $sslRequired;

  /**
   * @param string[]
   */
  public function setAdvertiserIds($advertiserIds)
  {
    $this->advertiserIds = $advertiserIds;
  }
  /**
   * @return string[]
   */
  public function getAdvertiserIds()
  {
    return $this->advertiserIds;
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
  public function setFloodlightActivityId($floodlightActivityId)
  {
    $this->floodlightActivityId = $floodlightActivityId;
  }
  /**
   * @return string
   */
  public function getFloodlightActivityId()
  {
    return $this->floodlightActivityId;
  }
  /**
   * @param string
   */
  public function setFloodlightGroupId($floodlightGroupId)
  {
    $this->floodlightGroupId = $floodlightGroupId;
  }
  /**
   * @return string
   */
  public function getFloodlightGroupId()
  {
    return $this->floodlightGroupId;
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
   * @param RemarketingConfig[]
   */
  public function setRemarketingConfigs($remarketingConfigs)
  {
    $this->remarketingConfigs = $remarketingConfigs;
  }
  /**
   * @return RemarketingConfig[]
   */
  public function getRemarketingConfigs()
  {
    return $this->remarketingConfigs;
  }
  /**
   * @param string
   */
  public function setServingStatus($servingStatus)
  {
    $this->servingStatus = $servingStatus;
  }
  /**
   * @return string
   */
  public function getServingStatus()
  {
    return $this->servingStatus;
  }
  /**
   * @param bool
   */
  public function setSslRequired($sslRequired)
  {
    $this->sslRequired = $sslRequired;
  }
  /**
   * @return bool
   */
  public function getSslRequired()
  {
    return $this->sslRequired;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(FloodlightActivity::class, 'Google_Service_DisplayVideo_FloodlightActivity');
