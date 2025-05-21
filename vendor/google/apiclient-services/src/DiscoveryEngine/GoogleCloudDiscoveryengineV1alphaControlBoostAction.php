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

namespace Google\Service\DiscoveryEngine;

class GoogleCloudDiscoveryengineV1alphaControlBoostAction extends \Google\Model
{
  /**
   * @var float
   */
  public $boost;
  /**
   * @var string
   */
  public $dataStore;
  /**
   * @var string
   */
  public $filter;
  /**
   * @var float
   */
  public $fixedBoost;
  protected $interpolationBoostSpecType = GoogleCloudDiscoveryengineV1alphaControlBoostActionInterpolationBoostSpec::class;
  protected $interpolationBoostSpecDataType = '';

  /**
   * @param float
   */
  public function setBoost($boost)
  {
    $this->boost = $boost;
  }
  /**
   * @return float
   */
  public function getBoost()
  {
    return $this->boost;
  }
  /**
   * @param string
   */
  public function setDataStore($dataStore)
  {
    $this->dataStore = $dataStore;
  }
  /**
   * @return string
   */
  public function getDataStore()
  {
    return $this->dataStore;
  }
  /**
   * @param string
   */
  public function setFilter($filter)
  {
    $this->filter = $filter;
  }
  /**
   * @return string
   */
  public function getFilter()
  {
    return $this->filter;
  }
  /**
   * @param float
   */
  public function setFixedBoost($fixedBoost)
  {
    $this->fixedBoost = $fixedBoost;
  }
  /**
   * @return float
   */
  public function getFixedBoost()
  {
    return $this->fixedBoost;
  }
  /**
   * @param GoogleCloudDiscoveryengineV1alphaControlBoostActionInterpolationBoostSpec
   */
  public function setInterpolationBoostSpec(GoogleCloudDiscoveryengineV1alphaControlBoostActionInterpolationBoostSpec $interpolationBoostSpec)
  {
    $this->interpolationBoostSpec = $interpolationBoostSpec;
  }
  /**
   * @return GoogleCloudDiscoveryengineV1alphaControlBoostActionInterpolationBoostSpec
   */
  public function getInterpolationBoostSpec()
  {
    return $this->interpolationBoostSpec;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GoogleCloudDiscoveryengineV1alphaControlBoostAction::class, 'Google_Service_DiscoveryEngine_GoogleCloudDiscoveryengineV1alphaControlBoostAction');
