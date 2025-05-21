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

namespace Google\Service\CloudDataplex;

class GoogleCloudDataplexV1DataQualityDimensionResult extends \Google\Model
{
  protected $dimensionType = GoogleCloudDataplexV1DataQualityDimension::class;
  protected $dimensionDataType = '';
  /**
   * @var bool
   */
  public $passed;
  /**
   * @var float
   */
  public $score;

  /**
   * @param GoogleCloudDataplexV1DataQualityDimension
   */
  public function setDimension(GoogleCloudDataplexV1DataQualityDimension $dimension)
  {
    $this->dimension = $dimension;
  }
  /**
   * @return GoogleCloudDataplexV1DataQualityDimension
   */
  public function getDimension()
  {
    return $this->dimension;
  }
  /**
   * @param bool
   */
  public function setPassed($passed)
  {
    $this->passed = $passed;
  }
  /**
   * @return bool
   */
  public function getPassed()
  {
    return $this->passed;
  }
  /**
   * @param float
   */
  public function setScore($score)
  {
    $this->score = $score;
  }
  /**
   * @return float
   */
  public function getScore()
  {
    return $this->score;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GoogleCloudDataplexV1DataQualityDimensionResult::class, 'Google_Service_CloudDataplex_GoogleCloudDataplexV1DataQualityDimensionResult');
