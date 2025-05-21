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

namespace Google\Service\Compute;

class InterconnectRemoteLocationConstraints extends \Google\Model
{
  /**
   * @var string
   */
  public $portPairRemoteLocation;
  /**
   * @var string
   */
  public $portPairVlan;
  protected $subnetLengthRangeType = InterconnectRemoteLocationConstraintsSubnetLengthRange::class;
  protected $subnetLengthRangeDataType = '';

  /**
   * @param string
   */
  public function setPortPairRemoteLocation($portPairRemoteLocation)
  {
    $this->portPairRemoteLocation = $portPairRemoteLocation;
  }
  /**
   * @return string
   */
  public function getPortPairRemoteLocation()
  {
    return $this->portPairRemoteLocation;
  }
  /**
   * @param string
   */
  public function setPortPairVlan($portPairVlan)
  {
    $this->portPairVlan = $portPairVlan;
  }
  /**
   * @return string
   */
  public function getPortPairVlan()
  {
    return $this->portPairVlan;
  }
  /**
   * @param InterconnectRemoteLocationConstraintsSubnetLengthRange
   */
  public function setSubnetLengthRange(InterconnectRemoteLocationConstraintsSubnetLengthRange $subnetLengthRange)
  {
    $this->subnetLengthRange = $subnetLengthRange;
  }
  /**
   * @return InterconnectRemoteLocationConstraintsSubnetLengthRange
   */
  public function getSubnetLengthRange()
  {
    return $this->subnetLengthRange;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(InterconnectRemoteLocationConstraints::class, 'Google_Service_Compute_InterconnectRemoteLocationConstraints');
