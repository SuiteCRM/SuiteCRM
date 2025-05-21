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

class SecurityPolicyRuleNetworkMatcher extends \Google\Collection
{
  protected $collection_key = 'userDefinedFields';
  /**
   * @var string[]
   */
  public $destIpRanges;
  /**
   * @var string[]
   */
  public $destPorts;
  /**
   * @var string[]
   */
  public $ipProtocols;
  /**
   * @var string[]
   */
  public $srcAsns;
  /**
   * @var string[]
   */
  public $srcIpRanges;
  /**
   * @var string[]
   */
  public $srcPorts;
  /**
   * @var string[]
   */
  public $srcRegionCodes;
  protected $userDefinedFieldsType = SecurityPolicyRuleNetworkMatcherUserDefinedFieldMatch::class;
  protected $userDefinedFieldsDataType = 'array';

  /**
   * @param string[]
   */
  public function setDestIpRanges($destIpRanges)
  {
    $this->destIpRanges = $destIpRanges;
  }
  /**
   * @return string[]
   */
  public function getDestIpRanges()
  {
    return $this->destIpRanges;
  }
  /**
   * @param string[]
   */
  public function setDestPorts($destPorts)
  {
    $this->destPorts = $destPorts;
  }
  /**
   * @return string[]
   */
  public function getDestPorts()
  {
    return $this->destPorts;
  }
  /**
   * @param string[]
   */
  public function setIpProtocols($ipProtocols)
  {
    $this->ipProtocols = $ipProtocols;
  }
  /**
   * @return string[]
   */
  public function getIpProtocols()
  {
    return $this->ipProtocols;
  }
  /**
   * @param string[]
   */
  public function setSrcAsns($srcAsns)
  {
    $this->srcAsns = $srcAsns;
  }
  /**
   * @return string[]
   */
  public function getSrcAsns()
  {
    return $this->srcAsns;
  }
  /**
   * @param string[]
   */
  public function setSrcIpRanges($srcIpRanges)
  {
    $this->srcIpRanges = $srcIpRanges;
  }
  /**
   * @return string[]
   */
  public function getSrcIpRanges()
  {
    return $this->srcIpRanges;
  }
  /**
   * @param string[]
   */
  public function setSrcPorts($srcPorts)
  {
    $this->srcPorts = $srcPorts;
  }
  /**
   * @return string[]
   */
  public function getSrcPorts()
  {
    return $this->srcPorts;
  }
  /**
   * @param string[]
   */
  public function setSrcRegionCodes($srcRegionCodes)
  {
    $this->srcRegionCodes = $srcRegionCodes;
  }
  /**
   * @return string[]
   */
  public function getSrcRegionCodes()
  {
    return $this->srcRegionCodes;
  }
  /**
   * @param SecurityPolicyRuleNetworkMatcherUserDefinedFieldMatch[]
   */
  public function setUserDefinedFields($userDefinedFields)
  {
    $this->userDefinedFields = $userDefinedFields;
  }
  /**
   * @return SecurityPolicyRuleNetworkMatcherUserDefinedFieldMatch[]
   */
  public function getUserDefinedFields()
  {
    return $this->userDefinedFields;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(SecurityPolicyRuleNetworkMatcher::class, 'Google_Service_Compute_SecurityPolicyRuleNetworkMatcher');
