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

namespace Google\Service\SQLAdmin;

class PscAutoConnectionConfig extends \Google\Model
{
  /**
   * @var string
   */
  public $consumerNetwork;
  /**
   * @var string
   */
  public $consumerNetworkStatus;
  /**
   * @var string
   */
  public $consumerProject;
  /**
   * @var string
   */
  public $ipAddress;
  /**
   * @var string
   */
  public $status;

  /**
   * @param string
   */
  public function setConsumerNetwork($consumerNetwork)
  {
    $this->consumerNetwork = $consumerNetwork;
  }
  /**
   * @return string
   */
  public function getConsumerNetwork()
  {
    return $this->consumerNetwork;
  }
  /**
   * @param string
   */
  public function setConsumerNetworkStatus($consumerNetworkStatus)
  {
    $this->consumerNetworkStatus = $consumerNetworkStatus;
  }
  /**
   * @return string
   */
  public function getConsumerNetworkStatus()
  {
    return $this->consumerNetworkStatus;
  }
  /**
   * @param string
   */
  public function setConsumerProject($consumerProject)
  {
    $this->consumerProject = $consumerProject;
  }
  /**
   * @return string
   */
  public function getConsumerProject()
  {
    return $this->consumerProject;
  }
  /**
   * @param string
   */
  public function setIpAddress($ipAddress)
  {
    $this->ipAddress = $ipAddress;
  }
  /**
   * @return string
   */
  public function getIpAddress()
  {
    return $this->ipAddress;
  }
  /**
   * @param string
   */
  public function setStatus($status)
  {
    $this->status = $status;
  }
  /**
   * @return string
   */
  public function getStatus()
  {
    return $this->status;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(PscAutoConnectionConfig::class, 'Google_Service_SQLAdmin_PscAutoConnectionConfig');
