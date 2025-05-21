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

class GoogleCloudAiplatformV1CompletionStats extends \Google\Model
{
  /**
   * @var string
   */
  public $failedCount;
  /**
   * @var string
   */
  public $incompleteCount;
  /**
   * @var string
   */
  public $successfulCount;
  /**
   * @var string
   */
  public $successfulForecastPointCount;

  /**
   * @param string
   */
  public function setFailedCount($failedCount)
  {
    $this->failedCount = $failedCount;
  }
  /**
   * @return string
   */
  public function getFailedCount()
  {
    return $this->failedCount;
  }
  /**
   * @param string
   */
  public function setIncompleteCount($incompleteCount)
  {
    $this->incompleteCount = $incompleteCount;
  }
  /**
   * @return string
   */
  public function getIncompleteCount()
  {
    return $this->incompleteCount;
  }
  /**
   * @param string
   */
  public function setSuccessfulCount($successfulCount)
  {
    $this->successfulCount = $successfulCount;
  }
  /**
   * @return string
   */
  public function getSuccessfulCount()
  {
    return $this->successfulCount;
  }
  /**
   * @param string
   */
  public function setSuccessfulForecastPointCount($successfulForecastPointCount)
  {
    $this->successfulForecastPointCount = $successfulForecastPointCount;
  }
  /**
   * @return string
   */
  public function getSuccessfulForecastPointCount()
  {
    return $this->successfulForecastPointCount;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GoogleCloudAiplatformV1CompletionStats::class, 'Google_Service_Aiplatform_GoogleCloudAiplatformV1CompletionStats');
