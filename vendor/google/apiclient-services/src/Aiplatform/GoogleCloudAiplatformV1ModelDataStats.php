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

class GoogleCloudAiplatformV1ModelDataStats extends \Google\Model
{
  /**
   * @var string
   */
  public $testAnnotationsCount;
  /**
   * @var string
   */
  public $testDataItemsCount;
  /**
   * @var string
   */
  public $trainingAnnotationsCount;
  /**
   * @var string
   */
  public $trainingDataItemsCount;
  /**
   * @var string
   */
  public $validationAnnotationsCount;
  /**
   * @var string
   */
  public $validationDataItemsCount;

  /**
   * @param string
   */
  public function setTestAnnotationsCount($testAnnotationsCount)
  {
    $this->testAnnotationsCount = $testAnnotationsCount;
  }
  /**
   * @return string
   */
  public function getTestAnnotationsCount()
  {
    return $this->testAnnotationsCount;
  }
  /**
   * @param string
   */
  public function setTestDataItemsCount($testDataItemsCount)
  {
    $this->testDataItemsCount = $testDataItemsCount;
  }
  /**
   * @return string
   */
  public function getTestDataItemsCount()
  {
    return $this->testDataItemsCount;
  }
  /**
   * @param string
   */
  public function setTrainingAnnotationsCount($trainingAnnotationsCount)
  {
    $this->trainingAnnotationsCount = $trainingAnnotationsCount;
  }
  /**
   * @return string
   */
  public function getTrainingAnnotationsCount()
  {
    return $this->trainingAnnotationsCount;
  }
  /**
   * @param string
   */
  public function setTrainingDataItemsCount($trainingDataItemsCount)
  {
    $this->trainingDataItemsCount = $trainingDataItemsCount;
  }
  /**
   * @return string
   */
  public function getTrainingDataItemsCount()
  {
    return $this->trainingDataItemsCount;
  }
  /**
   * @param string
   */
  public function setValidationAnnotationsCount($validationAnnotationsCount)
  {
    $this->validationAnnotationsCount = $validationAnnotationsCount;
  }
  /**
   * @return string
   */
  public function getValidationAnnotationsCount()
  {
    return $this->validationAnnotationsCount;
  }
  /**
   * @param string
   */
  public function setValidationDataItemsCount($validationDataItemsCount)
  {
    $this->validationDataItemsCount = $validationDataItemsCount;
  }
  /**
   * @return string
   */
  public function getValidationDataItemsCount()
  {
    return $this->validationDataItemsCount;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GoogleCloudAiplatformV1ModelDataStats::class, 'Google_Service_Aiplatform_GoogleCloudAiplatformV1ModelDataStats');
