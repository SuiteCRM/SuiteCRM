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

namespace Google\Service\DatabaseMigrationService;

class SequenceEntity extends \Google\Model
{
  /**
   * @var string
   */
  public $cache;
  /**
   * @var array[]
   */
  public $customFeatures;
  /**
   * @var bool
   */
  public $cycle;
  /**
   * @var string
   */
  public $increment;
  /**
   * @var string
   */
  public $maxValue;
  /**
   * @var string
   */
  public $minValue;
  /**
   * @var string
   */
  public $startValue;

  /**
   * @param string
   */
  public function setCache($cache)
  {
    $this->cache = $cache;
  }
  /**
   * @return string
   */
  public function getCache()
  {
    return $this->cache;
  }
  /**
   * @param array[]
   */
  public function setCustomFeatures($customFeatures)
  {
    $this->customFeatures = $customFeatures;
  }
  /**
   * @return array[]
   */
  public function getCustomFeatures()
  {
    return $this->customFeatures;
  }
  /**
   * @param bool
   */
  public function setCycle($cycle)
  {
    $this->cycle = $cycle;
  }
  /**
   * @return bool
   */
  public function getCycle()
  {
    return $this->cycle;
  }
  /**
   * @param string
   */
  public function setIncrement($increment)
  {
    $this->increment = $increment;
  }
  /**
   * @return string
   */
  public function getIncrement()
  {
    return $this->increment;
  }
  /**
   * @param string
   */
  public function setMaxValue($maxValue)
  {
    $this->maxValue = $maxValue;
  }
  /**
   * @return string
   */
  public function getMaxValue()
  {
    return $this->maxValue;
  }
  /**
   * @param string
   */
  public function setMinValue($minValue)
  {
    $this->minValue = $minValue;
  }
  /**
   * @return string
   */
  public function getMinValue()
  {
    return $this->minValue;
  }
  /**
   * @param string
   */
  public function setStartValue($startValue)
  {
    $this->startValue = $startValue;
  }
  /**
   * @return string
   */
  public function getStartValue()
  {
    return $this->startValue;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(SequenceEntity::class, 'Google_Service_DatabaseMigrationService_SequenceEntity');
