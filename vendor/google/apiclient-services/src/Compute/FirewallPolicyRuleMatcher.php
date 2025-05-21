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

class FirewallPolicyRuleMatcher extends \Google\Collection
{
  protected $collection_key = 'srcThreatIntelligences';
  /**
   * @var string[]
   */
  public $destAddressGroups;
  /**
   * @var string[]
   */
  public $destFqdns;
  /**
   * @var string[]
   */
  public $destIpRanges;
  /**
   * @var string[]
   */
  public $destRegionCodes;
  /**
   * @var string[]
   */
  public $destThreatIntelligences;
  protected $layer4ConfigsType = FirewallPolicyRuleMatcherLayer4Config::class;
  protected $layer4ConfigsDataType = 'array';
  /**
   * @var string[]
   */
  public $srcAddressGroups;
  /**
   * @var string[]
   */
  public $srcFqdns;
  /**
   * @var string[]
   */
  public $srcIpRanges;
  /**
   * @var string[]
   */
  public $srcRegionCodes;
  protected $srcSecureTagsType = FirewallPolicyRuleSecureTag::class;
  protected $srcSecureTagsDataType = 'array';
  /**
   * @var string[]
   */
  public $srcThreatIntelligences;

  /**
   * @param string[]
   */
  public function setDestAddressGroups($destAddressGroups)
  {
    $this->destAddressGroups = $destAddressGroups;
  }
  /**
   * @return string[]
   */
  public function getDestAddressGroups()
  {
    return $this->destAddressGroups;
  }
  /**
   * @param string[]
   */
  public function setDestFqdns($destFqdns)
  {
    $this->destFqdns = $destFqdns;
  }
  /**
   * @return string[]
   */
  public function getDestFqdns()
  {
    return $this->destFqdns;
  }
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
  public function setDestRegionCodes($destRegionCodes)
  {
    $this->destRegionCodes = $destRegionCodes;
  }
  /**
   * @return string[]
   */
  public function getDestRegionCodes()
  {
    return $this->destRegionCodes;
  }
  /**
   * @param string[]
   */
  public function setDestThreatIntelligences($destThreatIntelligences)
  {
    $this->destThreatIntelligences = $destThreatIntelligences;
  }
  /**
   * @return string[]
   */
  public function getDestThreatIntelligences()
  {
    return $this->destThreatIntelligences;
  }
  /**
   * @param FirewallPolicyRuleMatcherLayer4Config[]
   */
  public function setLayer4Configs($layer4Configs)
  {
    $this->layer4Configs = $layer4Configs;
  }
  /**
   * @return FirewallPolicyRuleMatcherLayer4Config[]
   */
  public function getLayer4Configs()
  {
    return $this->layer4Configs;
  }
  /**
   * @param string[]
   */
  public function setSrcAddressGroups($srcAddressGroups)
  {
    $this->srcAddressGroups = $srcAddressGroups;
  }
  /**
   * @return string[]
   */
  public function getSrcAddressGroups()
  {
    return $this->srcAddressGroups;
  }
  /**
   * @param string[]
   */
  public function setSrcFqdns($srcFqdns)
  {
    $this->srcFqdns = $srcFqdns;
  }
  /**
   * @return string[]
   */
  public function getSrcFqdns()
  {
    return $this->srcFqdns;
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
   * @param FirewallPolicyRuleSecureTag[]
   */
  public function setSrcSecureTags($srcSecureTags)
  {
    $this->srcSecureTags = $srcSecureTags;
  }
  /**
   * @return FirewallPolicyRuleSecureTag[]
   */
  public function getSrcSecureTags()
  {
    return $this->srcSecureTags;
  }
  /**
   * @param string[]
   */
  public function setSrcThreatIntelligences($srcThreatIntelligences)
  {
    $this->srcThreatIntelligences = $srcThreatIntelligences;
  }
  /**
   * @return string[]
   */
  public function getSrcThreatIntelligences()
  {
    return $this->srcThreatIntelligences;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(FirewallPolicyRuleMatcher::class, 'Google_Service_Compute_FirewallPolicyRuleMatcher');
