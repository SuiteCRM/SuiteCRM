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

class SecurityPolicyRulePreconfiguredWafConfigExclusion extends \Google\Collection
{
  protected $collection_key = 'targetRuleIds';
  protected $requestCookiesToExcludeType = SecurityPolicyRulePreconfiguredWafConfigExclusionFieldParams::class;
  protected $requestCookiesToExcludeDataType = 'array';
  protected $requestHeadersToExcludeType = SecurityPolicyRulePreconfiguredWafConfigExclusionFieldParams::class;
  protected $requestHeadersToExcludeDataType = 'array';
  protected $requestQueryParamsToExcludeType = SecurityPolicyRulePreconfiguredWafConfigExclusionFieldParams::class;
  protected $requestQueryParamsToExcludeDataType = 'array';
  protected $requestUrisToExcludeType = SecurityPolicyRulePreconfiguredWafConfigExclusionFieldParams::class;
  protected $requestUrisToExcludeDataType = 'array';
  /**
   * @var string[]
   */
  public $targetRuleIds;
  /**
   * @var string
   */
  public $targetRuleSet;

  /**
   * @param SecurityPolicyRulePreconfiguredWafConfigExclusionFieldParams[]
   */
  public function setRequestCookiesToExclude($requestCookiesToExclude)
  {
    $this->requestCookiesToExclude = $requestCookiesToExclude;
  }
  /**
   * @return SecurityPolicyRulePreconfiguredWafConfigExclusionFieldParams[]
   */
  public function getRequestCookiesToExclude()
  {
    return $this->requestCookiesToExclude;
  }
  /**
   * @param SecurityPolicyRulePreconfiguredWafConfigExclusionFieldParams[]
   */
  public function setRequestHeadersToExclude($requestHeadersToExclude)
  {
    $this->requestHeadersToExclude = $requestHeadersToExclude;
  }
  /**
   * @return SecurityPolicyRulePreconfiguredWafConfigExclusionFieldParams[]
   */
  public function getRequestHeadersToExclude()
  {
    return $this->requestHeadersToExclude;
  }
  /**
   * @param SecurityPolicyRulePreconfiguredWafConfigExclusionFieldParams[]
   */
  public function setRequestQueryParamsToExclude($requestQueryParamsToExclude)
  {
    $this->requestQueryParamsToExclude = $requestQueryParamsToExclude;
  }
  /**
   * @return SecurityPolicyRulePreconfiguredWafConfigExclusionFieldParams[]
   */
  public function getRequestQueryParamsToExclude()
  {
    return $this->requestQueryParamsToExclude;
  }
  /**
   * @param SecurityPolicyRulePreconfiguredWafConfigExclusionFieldParams[]
   */
  public function setRequestUrisToExclude($requestUrisToExclude)
  {
    $this->requestUrisToExclude = $requestUrisToExclude;
  }
  /**
   * @return SecurityPolicyRulePreconfiguredWafConfigExclusionFieldParams[]
   */
  public function getRequestUrisToExclude()
  {
    return $this->requestUrisToExclude;
  }
  /**
   * @param string[]
   */
  public function setTargetRuleIds($targetRuleIds)
  {
    $this->targetRuleIds = $targetRuleIds;
  }
  /**
   * @return string[]
   */
  public function getTargetRuleIds()
  {
    return $this->targetRuleIds;
  }
  /**
   * @param string
   */
  public function setTargetRuleSet($targetRuleSet)
  {
    $this->targetRuleSet = $targetRuleSet;
  }
  /**
   * @return string
   */
  public function getTargetRuleSet()
  {
    return $this->targetRuleSet;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(SecurityPolicyRulePreconfiguredWafConfigExclusion::class, 'Google_Service_Compute_SecurityPolicyRulePreconfiguredWafConfigExclusion');
