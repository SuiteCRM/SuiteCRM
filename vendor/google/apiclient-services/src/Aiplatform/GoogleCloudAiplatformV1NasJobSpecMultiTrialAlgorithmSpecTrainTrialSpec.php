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

class GoogleCloudAiplatformV1NasJobSpecMultiTrialAlgorithmSpecTrainTrialSpec extends \Google\Model
{
  /**
   * @var int
   */
  public $frequency;
  /**
   * @var int
   */
  public $maxParallelTrialCount;
  protected $trainTrialJobSpecType = GoogleCloudAiplatformV1CustomJobSpec::class;
  protected $trainTrialJobSpecDataType = '';

  /**
   * @param int
   */
  public function setFrequency($frequency)
  {
    $this->frequency = $frequency;
  }
  /**
   * @return int
   */
  public function getFrequency()
  {
    return $this->frequency;
  }
  /**
   * @param int
   */
  public function setMaxParallelTrialCount($maxParallelTrialCount)
  {
    $this->maxParallelTrialCount = $maxParallelTrialCount;
  }
  /**
   * @return int
   */
  public function getMaxParallelTrialCount()
  {
    return $this->maxParallelTrialCount;
  }
  /**
   * @param GoogleCloudAiplatformV1CustomJobSpec
   */
  public function setTrainTrialJobSpec(GoogleCloudAiplatformV1CustomJobSpec $trainTrialJobSpec)
  {
    $this->trainTrialJobSpec = $trainTrialJobSpec;
  }
  /**
   * @return GoogleCloudAiplatformV1CustomJobSpec
   */
  public function getTrainTrialJobSpec()
  {
    return $this->trainTrialJobSpec;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GoogleCloudAiplatformV1NasJobSpecMultiTrialAlgorithmSpecTrainTrialSpec::class, 'Google_Service_Aiplatform_GoogleCloudAiplatformV1NasJobSpecMultiTrialAlgorithmSpecTrainTrialSpec');
