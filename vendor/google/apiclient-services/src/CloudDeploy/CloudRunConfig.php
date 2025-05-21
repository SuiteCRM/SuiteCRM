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

namespace Google\Service\CloudDeploy;

class CloudRunConfig extends \Google\Collection
{
  protected $collection_key = 'stableRevisionTags';
  /**
   * @var bool
   */
  public $automaticTrafficControl;
  /**
   * @var string[]
   */
  public $canaryRevisionTags;
  /**
   * @var string[]
   */
  public $priorRevisionTags;
  /**
   * @var string[]
   */
  public $stableRevisionTags;

  /**
   * @param bool
   */
  public function setAutomaticTrafficControl($automaticTrafficControl)
  {
    $this->automaticTrafficControl = $automaticTrafficControl;
  }
  /**
   * @return bool
   */
  public function getAutomaticTrafficControl()
  {
    return $this->automaticTrafficControl;
  }
  /**
   * @param string[]
   */
  public function setCanaryRevisionTags($canaryRevisionTags)
  {
    $this->canaryRevisionTags = $canaryRevisionTags;
  }
  /**
   * @return string[]
   */
  public function getCanaryRevisionTags()
  {
    return $this->canaryRevisionTags;
  }
  /**
   * @param string[]
   */
  public function setPriorRevisionTags($priorRevisionTags)
  {
    $this->priorRevisionTags = $priorRevisionTags;
  }
  /**
   * @return string[]
   */
  public function getPriorRevisionTags()
  {
    return $this->priorRevisionTags;
  }
  /**
   * @param string[]
   */
  public function setStableRevisionTags($stableRevisionTags)
  {
    $this->stableRevisionTags = $stableRevisionTags;
  }
  /**
   * @return string[]
   */
  public function getStableRevisionTags()
  {
    return $this->stableRevisionTags;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(CloudRunConfig::class, 'Google_Service_CloudDeploy_CloudRunConfig');
