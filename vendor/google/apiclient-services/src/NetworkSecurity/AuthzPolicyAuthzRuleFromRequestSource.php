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

class AuthzPolicyAuthzRuleFromRequestSource extends \Google\Collection
{
  protected $collection_key = 'resources';
  protected $principalsType = AuthzPolicyAuthzRuleStringMatch::class;
  protected $principalsDataType = 'array';
  protected $resourcesType = AuthzPolicyAuthzRuleRequestResource::class;
  protected $resourcesDataType = 'array';

  /**
   * @param AuthzPolicyAuthzRuleStringMatch[]
   */
  public function setPrincipals($principals)
  {
    $this->principals = $principals;
  }
  /**
   * @return AuthzPolicyAuthzRuleStringMatch[]
   */
  public function getPrincipals()
  {
    return $this->principals;
  }
  /**
   * @param AuthzPolicyAuthzRuleRequestResource[]
   */
  public function setResources($resources)
  {
    $this->resources = $resources;
  }
  /**
   * @return AuthzPolicyAuthzRuleRequestResource[]
   */
  public function getResources()
  {
    return $this->resources;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(AuthzPolicyAuthzRuleFromRequestSource::class, 'Google_Service_NetworkSecurity_AuthzPolicyAuthzRuleFromRequestSource');
