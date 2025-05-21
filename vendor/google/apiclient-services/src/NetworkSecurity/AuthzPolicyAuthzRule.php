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

namespace Google\Service\NetworkSecurity;

class AuthzPolicyAuthzRule extends \Google\Model
{
  protected $fromType = AuthzPolicyAuthzRuleFrom::class;
  protected $fromDataType = '';
  protected $toType = AuthzPolicyAuthzRuleTo::class;
  protected $toDataType = '';
  /**
   * @var string
   */
  public $when;

  /**
   * @param AuthzPolicyAuthzRuleFrom
   */
  public function setFrom(AuthzPolicyAuthzRuleFrom $from)
  {
    $this->from = $from;
  }
  /**
   * @return AuthzPolicyAuthzRuleFrom
   */
  public function getFrom()
  {
    return $this->from;
  }
  /**
   * @param AuthzPolicyAuthzRuleTo
   */
  public function setTo(AuthzPolicyAuthzRuleTo $to)
  {
    $this->to = $to;
  }
  /**
   * @return AuthzPolicyAuthzRuleTo
   */
  public function getTo()
  {
    return $this->to;
  }
  /**
   * @param string
   */
  public function setWhen($when)
  {
    $this->when = $when;
  }
  /**
   * @return string
   */
  public function getWhen()
  {
    return $this->when;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(AuthzPolicyAuthzRule::class, 'Google_Service_NetworkSecurity_AuthzPolicyAuthzRule');
