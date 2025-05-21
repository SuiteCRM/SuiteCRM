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

class GoogleCloudAiplatformV1PipelineTaskExecutorDetailContainerDetail extends \Google\Collection
{
  protected $collection_key = 'failedPreCachingCheckJobs';
  /**
   * @var string[]
   */
  public $failedMainJobs;
  /**
   * @var string[]
   */
  public $failedPreCachingCheckJobs;
  /**
   * @var string
   */
  public $mainJob;
  /**
   * @var string
   */
  public $preCachingCheckJob;

  /**
   * @param string[]
   */
  public function setFailedMainJobs($failedMainJobs)
  {
    $this->failedMainJobs = $failedMainJobs;
  }
  /**
   * @return string[]
   */
  public function getFailedMainJobs()
  {
    return $this->failedMainJobs;
  }
  /**
   * @param string[]
   */
  public function setFailedPreCachingCheckJobs($failedPreCachingCheckJobs)
  {
    $this->failedPreCachingCheckJobs = $failedPreCachingCheckJobs;
  }
  /**
   * @return string[]
   */
  public function getFailedPreCachingCheckJobs()
  {
    return $this->failedPreCachingCheckJobs;
  }
  /**
   * @param string
   */
  public function setMainJob($mainJob)
  {
    $this->mainJob = $mainJob;
  }
  /**
   * @return string
   */
  public function getMainJob()
  {
    return $this->mainJob;
  }
  /**
   * @param string
   */
  public function setPreCachingCheckJob($preCachingCheckJob)
  {
    $this->preCachingCheckJob = $preCachingCheckJob;
  }
  /**
   * @return string
   */
  public function getPreCachingCheckJob()
  {
    return $this->preCachingCheckJob;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GoogleCloudAiplatformV1PipelineTaskExecutorDetailContainerDetail::class, 'Google_Service_Aiplatform_GoogleCloudAiplatformV1PipelineTaskExecutorDetailContainerDetail');
