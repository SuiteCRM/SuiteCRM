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

namespace Google\Service\Spanner;

class AutoscalingLimits extends \Google\Model
{
  /**
   * @var int
   */
  public $maxNodes;
  /**
   * @var int
   */
  public $maxProcessingUnits;
  /**
   * @var int
   */
  public $minNodes;
  /**
   * @var int
   */
  public $minProcessingUnits;

  /**
   * @param int
   */
  public function setMaxNodes($maxNodes)
  {
    $this->maxNodes = $maxNodes;
  }
  /**
   * @return int
   */
  public function getMaxNodes()
  {
    return $this->maxNodes;
  }
  /**
   * @param int
   */
  public function setMaxProcessingUnits($maxProcessingUnits)
  {
    $this->maxProcessingUnits = $maxProcessingUnits;
  }
  /**
   * @return int
   */
  public function getMaxProcessingUnits()
  {
    return $this->maxProcessingUnits;
  }
  /**
   * @param int
   */
  public function setMinNodes($minNodes)
  {
    $this->minNodes = $minNodes;
  }
  /**
   * @return int
   */
  public function getMinNodes()
  {
    return $this->minNodes;
  }
  /**
   * @param int
   */
  public function setMinProcessingUnits($minProcessingUnits)
  {
    $this->minProcessingUnits = $minProcessingUnits;
  }
  /**
   * @return int
   */
  public function getMinProcessingUnits()
  {
    return $this->minProcessingUnits;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(AutoscalingLimits::class, 'Google_Service_Spanner_AutoscalingLimits');
