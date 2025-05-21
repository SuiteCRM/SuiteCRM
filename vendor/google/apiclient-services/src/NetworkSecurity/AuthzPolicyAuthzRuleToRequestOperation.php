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

class AuthzPolicyAuthzRuleToRequestOperation extends \Google\Collection
{
  protected $collection_key = 'paths';
  protected $headerSetType = AuthzPolicyAuthzRuleToRequestOperationHeaderSet::class;
  protected $headerSetDataType = '';
  protected $hostsType = AuthzPolicyAuthzRuleStringMatch::class;
  protected $hostsDataType = 'array';
  /**
   * @var string[]
   */
  public $methods;
  protected $pathsType = AuthzPolicyAuthzRuleStringMatch::class;
  protected $pathsDataType = 'array';

  /**
   * @param AuthzPolicyAuthzRuleToRequestOperationHeaderSet
   */
  public function setHeaderSet(AuthzPolicyAuthzRuleToRequestOperationHeaderSet $headerSet)
  {
    $this->headerSet = $headerSet;
  }
  /**
   * @return AuthzPolicyAuthzRuleToRequestOperationHeaderSet
   */
  public function getHeaderSet()
  {
    return $this->headerSet;
  }
  /**
   * @param AuthzPolicyAuthzRuleStringMatch[]
   */
  public function setHosts($hosts)
  {
    $this->hosts = $hosts;
  }
  /**
   * @return AuthzPolicyAuthzRuleStringMatch[]
   */
  public function getHosts()
  {
    return $this->hosts;
  }
  /**
   * @param string[]
   */
  public function setMethods($methods)
  {
    $this->methods = $methods;
  }
  /**
   * @return string[]
   */
  public function getMethods()
  {
    return $this->methods;
  }
  /**
   * @param AuthzPolicyAuthzRuleStringMatch[]
   */
  public function setPaths($paths)
  {
    $this->paths = $paths;
  }
  /**
   * @return AuthzPolicyAuthzRuleStringMatch[]
   */
  public function getPaths()
  {
    return $this->paths;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(AuthzPolicyAuthzRuleToRequestOperation::class, 'Google_Service_NetworkSecurity_AuthzPolicyAuthzRuleToRequestOperation');
