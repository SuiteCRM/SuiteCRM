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

class GoogleCloudAiplatformV1NasJobSpec extends \Google\Model
{
  protected $multiTrialAlgorithmSpecType = GoogleCloudAiplatformV1NasJobSpecMultiTrialAlgorithmSpec::class;
  protected $multiTrialAlgorithmSpecDataType = '';
  /**
   * @var string
   */
  public $resumeNasJobId;
  /**
   * @var string
   */
  public $searchSpaceSpec;

  /**
   * @param GoogleCloudAiplatformV1NasJobSpecMultiTrialAlgorithmSpec
   */
  public function setMultiTrialAlgorithmSpec(GoogleCloudAiplatformV1NasJobSpecMultiTrialAlgorithmSpec $multiTrialAlgorithmSpec)
  {
    $this->multiTrialAlgorithmSpec = $multiTrialAlgorithmSpec;
  }
  /**
   * @return GoogleCloudAiplatformV1NasJobSpecMultiTrialAlgorithmSpec
   */
  public function getMultiTrialAlgorithmSpec()
  {
    return $this->multiTrialAlgorithmSpec;
  }
  /**
   * @param string
   */
  public function setResumeNasJobId($resumeNasJobId)
  {
    $this->resumeNasJobId = $resumeNasJobId;
  }
  /**
   * @return string
   */
  public function getResumeNasJobId()
  {
    return $this->resumeNasJobId;
  }
  /**
   * @param string
   */
  public function setSearchSpaceSpec($searchSpaceSpec)
  {
    $this->searchSpaceSpec = $searchSpaceSpec;
  }
  /**
   * @return string
   */
  public function getSearchSpaceSpec()
  {
    return $this->searchSpaceSpec;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GoogleCloudAiplatformV1NasJobSpec::class, 'Google_Service_Aiplatform_GoogleCloudAiplatformV1NasJobSpec');
