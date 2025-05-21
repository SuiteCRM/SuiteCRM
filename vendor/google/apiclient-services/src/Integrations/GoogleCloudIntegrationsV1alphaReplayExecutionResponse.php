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

namespace Google\Service\Integrations;

class GoogleCloudIntegrationsV1alphaReplayExecutionResponse extends \Google\Model
{
  /**
   * @var string
   */
  public $executionId;
  /**
   * @var array[]
   */
  public $outputParameters;
  /**
   * @var string
   */
  public $replayedExecutionId;

  /**
   * @param string
   */
  public function setExecutionId($executionId)
  {
    $this->executionId = $executionId;
  }
  /**
   * @return string
   */
  public function getExecutionId()
  {
    return $this->executionId;
  }
  /**
   * @param array[]
   */
  public function setOutputParameters($outputParameters)
  {
    $this->outputParameters = $outputParameters;
  }
  /**
   * @return array[]
   */
  public function getOutputParameters()
  {
    return $this->outputParameters;
  }
  /**
   * @param string
   */
  public function setReplayedExecutionId($replayedExecutionId)
  {
    $this->replayedExecutionId = $replayedExecutionId;
  }
  /**
   * @return string
   */
  public function getReplayedExecutionId()
  {
    return $this->replayedExecutionId;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GoogleCloudIntegrationsV1alphaReplayExecutionResponse::class, 'Google_Service_Integrations_GoogleCloudIntegrationsV1alphaReplayExecutionResponse');
