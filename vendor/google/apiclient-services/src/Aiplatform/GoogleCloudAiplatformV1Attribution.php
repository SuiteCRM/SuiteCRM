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

namespace Google\Service\Aiplatform;

class GoogleCloudAiplatformV1Attribution extends \Google\Collection
{
  protected $collection_key = 'outputIndex';
  public $approximationError;
  public $baselineOutputValue;
  /**
   * @var array
   */
  public $featureAttributions;
  public $instanceOutputValue;
  /**
   * @var string
   */
  public $outputDisplayName;
  /**
   * @var int[]
   */
  public $outputIndex;
  /**
   * @var string
   */
  public $outputName;

  public function setApproximationError($approximationError)
  {
    $this->approximationError = $approximationError;
  }
  public function getApproximationError()
  {
    return $this->approximationError;
  }
  public function setBaselineOutputValue($baselineOutputValue)
  {
    $this->baselineOutputValue = $baselineOutputValue;
  }
  public function getBaselineOutputValue()
  {
    return $this->baselineOutputValue;
  }
  /**
   * @param array
   */
  public function setFeatureAttributions($featureAttributions)
  {
    $this->featureAttributions = $featureAttributions;
  }
  /**
   * @return array
   */
  public function getFeatureAttributions()
  {
    return $this->featureAttributions;
  }
  public function setInstanceOutputValue($instanceOutputValue)
  {
    $this->instanceOutputValue = $instanceOutputValue;
  }
  public function getInstanceOutputValue()
  {
    return $this->instanceOutputValue;
  }
  /**
   * @param string
   */
  public function setOutputDisplayName($outputDisplayName)
  {
    $this->outputDisplayName = $outputDisplayName;
  }
  /**
   * @return string
   */
  public function getOutputDisplayName()
  {
    return $this->outputDisplayName;
  }
  /**
   * @param int[]
   */
  public function setOutputIndex($outputIndex)
  {
    $this->outputIndex = $outputIndex;
  }
  /**
   * @return int[]
   */
  public function getOutputIndex()
  {
    return $this->outputIndex;
  }
  /**
   * @param string
   */
  public function setOutputName($outputName)
  {
    $this->outputName = $outputName;
  }
  /**
   * @return string
   */
  public function getOutputName()
  {
    return $this->outputName;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GoogleCloudAiplatformV1Attribution::class, 'Google_Service_Aiplatform_GoogleCloudAiplatformV1Attribution');
