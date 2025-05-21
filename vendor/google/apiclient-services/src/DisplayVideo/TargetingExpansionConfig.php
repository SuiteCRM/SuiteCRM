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

class TargetingExpansionConfig extends \Google\Model
{
  /**
   * @var string
   */
  public $audienceExpansionLevel;
  /**
   * @var bool
   */
  public $audienceExpansionSeedListExcluded;
  /**
   * @var bool
   */
  public $enableOptimizedTargeting;

  /**
   * @param string
   */
  public function setAudienceExpansionLevel($audienceExpansionLevel)
  {
    $this->audienceExpansionLevel = $audienceExpansionLevel;
  }
  /**
   * @return string
   */
  public function getAudienceExpansionLevel()
  {
    return $this->audienceExpansionLevel;
  }
  /**
   * @param bool
   */
  public function setAudienceExpansionSeedListExcluded($audienceExpansionSeedListExcluded)
  {
    $this->audienceExpansionSeedListExcluded = $audienceExpansionSeedListExcluded;
  }
  /**
   * @return bool
   */
  public function getAudienceExpansionSeedListExcluded()
  {
    return $this->audienceExpansionSeedListExcluded;
  }
  /**
   * @param bool
   */
  public function setEnableOptimizedTargeting($enableOptimizedTargeting)
  {
    $this->enableOptimizedTargeting = $enableOptimizedTargeting;
  }
  /**
   * @return bool
   */
  public function getEnableOptimizedTargeting()
  {
    return $this->enableOptimizedTargeting;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(TargetingExpansionConfig::class, 'Google_Service_DisplayVideo_TargetingExpansionConfig');
