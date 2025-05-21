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

class GoogleCloudAiplatformV1TimestampSplit extends \Google\Model
{
  /**
   * @var string
   */
  public $key;
  public $testFraction;
  public $trainingFraction;
  public $validationFraction;

  /**
   * @param string
   */
  public function setKey($key)
  {
    $this->key = $key;
  }
  /**
   * @return string
   */
  public function getKey()
  {
    return $this->key;
  }
  public function setTestFraction($testFraction)
  {
    $this->testFraction = $testFraction;
  }
  public function getTestFraction()
  {
    return $this->testFraction;
  }
  public function setTrainingFraction($trainingFraction)
  {
    $this->trainingFraction = $trainingFraction;
  }
  public function getTrainingFraction()
  {
    return $this->trainingFraction;
  }
  public function setValidationFraction($validationFraction)
  {
    $this->validationFraction = $validationFraction;
  }
  public function getValidationFraction()
  {
    return $this->validationFraction;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GoogleCloudAiplatformV1TimestampSplit::class, 'Google_Service_Aiplatform_GoogleCloudAiplatformV1TimestampSplit');
