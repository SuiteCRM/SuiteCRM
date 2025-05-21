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

class AuthzPolicyAuthzRuleTo extends \Google\Collection
{
  protected $collection_key = 'operations';
  protected $notOperationsType = AuthzPolicyAuthzRuleToRequestOperation::class;
  protected $notOperationsDataType = 'array';
  protected $operationsType = AuthzPolicyAuthzRuleToRequestOperation::class;
  protected $operationsDataType = 'array';

  /**
   * @param AuthzPolicyAuthzRuleToRequestOperation[]
   */
  public function setNotOperations($notOperations)
  {
    $this->notOperations = $notOperations;
  }
  /**
   * @return AuthzPolicyAuthzRuleToRequestOperation[]
   */
  public function getNotOperations()
  {
    return $this->notOperations;
  }
  /**
   * @param AuthzPolicyAuthzRuleToRequestOperation[]
   */
  public function setOperations($operations)
  {
    $this->operations = $operations;
  }
  /**
   * @return AuthzPolicyAuthzRuleToRequestOperation[]
   */
  public function getOperations()
  {
    return $this->operations;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(AuthzPolicyAuthzRuleTo::class, 'Google_Service_NetworkSecurity_AuthzPolicyAuthzRuleTo');
