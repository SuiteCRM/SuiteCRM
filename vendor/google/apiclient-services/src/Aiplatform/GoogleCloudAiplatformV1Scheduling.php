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

class GoogleCloudAiplatformV1Scheduling extends \Google\Model
{
  /**
   * @var bool
   */
  public $disableRetries;
  /**
   * @var string
   */
  public $maxWaitDuration;
  /**
   * @var bool
   */
  public $restartJobOnWorkerRestart;
  /**
   * @var string
   */
  public $strategy;
  /**
   * @var string
   */
  public $timeout;

  /**
   * @param bool
   */
  public function setDisableRetries($disableRetries)
  {
    $this->disableRetries = $disableRetries;
  }
  /**
   * @return bool
   */
  public function getDisableRetries()
  {
    return $this->disableRetries;
  }
  /**
   * @param string
   */
  public function setMaxWaitDuration($maxWaitDuration)
  {
    $this->maxWaitDuration = $maxWaitDuration;
  }
  /**
   * @return string
   */
  public function getMaxWaitDuration()
  {
    return $this->maxWaitDuration;
  }
  /**
   * @param bool
   */
  public function setRestartJobOnWorkerRestart($restartJobOnWorkerRestart)
  {
    $this->restartJobOnWorkerRestart = $restartJobOnWorkerRestart;
  }
  /**
   * @return bool
   */
  public function getRestartJobOnWorkerRestart()
  {
    return $this->restartJobOnWorkerRestart;
  }
  /**
   * @param string
   */
  public function setStrategy($strategy)
  {
    $this->strategy = $strategy;
  }
  /**
   * @return string
   */
  public function getStrategy()
  {
    return $this->strategy;
  }
  /**
   * @param string
   */
  public function setTimeout($timeout)
  {
    $this->timeout = $timeout;
  }
  /**
   * @return string
   */
  public function getTimeout()
  {
    return $this->timeout;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GoogleCloudAiplatformV1Scheduling::class, 'Google_Service_Aiplatform_GoogleCloudAiplatformV1Scheduling');
