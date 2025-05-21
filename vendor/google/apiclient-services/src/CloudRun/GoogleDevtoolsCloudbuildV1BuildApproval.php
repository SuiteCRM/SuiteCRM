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

namespace Google\Service\CloudRun;

class GoogleDevtoolsCloudbuildV1BuildApproval extends \Google\Model
{
  protected $configType = GoogleDevtoolsCloudbuildV1ApprovalConfig::class;
  protected $configDataType = '';
  protected $resultType = GoogleDevtoolsCloudbuildV1ApprovalResult::class;
  protected $resultDataType = '';
  /**
   * @var string
   */
  public $state;

  /**
   * @param GoogleDevtoolsCloudbuildV1ApprovalConfig
   */
  public function setConfig(GoogleDevtoolsCloudbuildV1ApprovalConfig $config)
  {
    $this->config = $config;
  }
  /**
   * @return GoogleDevtoolsCloudbuildV1ApprovalConfig
   */
  public function getConfig()
  {
    return $this->config;
  }
  /**
   * @param GoogleDevtoolsCloudbuildV1ApprovalResult
   */
  public function setResult(GoogleDevtoolsCloudbuildV1ApprovalResult $result)
  {
    $this->result = $result;
  }
  /**
   * @return GoogleDevtoolsCloudbuildV1ApprovalResult
   */
  public function getResult()
  {
    return $this->result;
  }
  /**
   * @param string
   */
  public function setState($state)
  {
    $this->state = $state;
  }
  /**
   * @return string
   */
  public function getState()
  {
    return $this->state;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GoogleDevtoolsCloudbuildV1BuildApproval::class, 'Google_Service_CloudRun_GoogleDevtoolsCloudbuildV1BuildApproval');
