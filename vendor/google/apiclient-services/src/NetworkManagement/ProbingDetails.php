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

namespace Google\Service\NetworkManagement;

class ProbingDetails extends \Google\Model
{
  /**
   * @var string
   */
  public $abortCause;
  protected $destinationEgressLocationType = EdgeLocation::class;
  protected $destinationEgressLocationDataType = '';
  protected $endpointInfoType = EndpointInfo::class;
  protected $endpointInfoDataType = '';
  protected $errorType = Status::class;
  protected $errorDataType = '';
  protected $probingLatencyType = LatencyDistribution::class;
  protected $probingLatencyDataType = '';
  /**
   * @var string
   */
  public $result;
  /**
   * @var int
   */
  public $sentProbeCount;
  /**
   * @var int
   */
  public $successfulProbeCount;
  /**
   * @var string
   */
  public $verifyTime;

  /**
   * @param string
   */
  public function setAbortCause($abortCause)
  {
    $this->abortCause = $abortCause;
  }
  /**
   * @return string
   */
  public function getAbortCause()
  {
    return $this->abortCause;
  }
  /**
   * @param EdgeLocation
   */
  public function setDestinationEgressLocation(EdgeLocation $destinationEgressLocation)
  {
    $this->destinationEgressLocation = $destinationEgressLocation;
  }
  /**
   * @return EdgeLocation
   */
  public function getDestinationEgressLocation()
  {
    return $this->destinationEgressLocation;
  }
  /**
   * @param EndpointInfo
   */
  public function setEndpointInfo(EndpointInfo $endpointInfo)
  {
    $this->endpointInfo = $endpointInfo;
  }
  /**
   * @return EndpointInfo
   */
  public function getEndpointInfo()
  {
    return $this->endpointInfo;
  }
  /**
   * @param Status
   */
  public function setError(Status $error)
  {
    $this->error = $error;
  }
  /**
   * @return Status
   */
  public function getError()
  {
    return $this->error;
  }
  /**
   * @param LatencyDistribution
   */
  public function setProbingLatency(LatencyDistribution $probingLatency)
  {
    $this->probingLatency = $probingLatency;
  }
  /**
   * @return LatencyDistribution
   */
  public function getProbingLatency()
  {
    return $this->probingLatency;
  }
  /**
   * @param string
   */
  public function setResult($result)
  {
    $this->result = $result;
  }
  /**
   * @return string
   */
  public function getResult()
  {
    return $this->result;
  }
  /**
   * @param int
   */
  public function setSentProbeCount($sentProbeCount)
  {
    $this->sentProbeCount = $sentProbeCount;
  }
  /**
   * @return int
   */
  public function getSentProbeCount()
  {
    return $this->sentProbeCount;
  }
  /**
   * @param int
   */
  public function setSuccessfulProbeCount($successfulProbeCount)
  {
    $this->successfulProbeCount = $successfulProbeCount;
  }
  /**
   * @return int
   */
  public function getSuccessfulProbeCount()
  {
    return $this->successfulProbeCount;
  }
  /**
   * @param string
   */
  public function setVerifyTime($verifyTime)
  {
    $this->verifyTime = $verifyTime;
  }
  /**
   * @return string
   */
  public function getVerifyTime()
  {
    return $this->verifyTime;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(ProbingDetails::class, 'Google_Service_NetworkManagement_ProbingDetails');
