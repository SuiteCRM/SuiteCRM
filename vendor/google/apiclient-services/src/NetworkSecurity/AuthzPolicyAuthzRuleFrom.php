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

class AuthzPolicyAuthzRuleFrom extends \Google\Collection
{
  protected $collection_key = 'sources';
  protected $notSourcesType = AuthzPolicyAuthzRuleFromRequestSource::class;
  protected $notSourcesDataType = 'array';
  protected $sourcesType = AuthzPolicyAuthzRuleFromRequestSource::class;
  protected $sourcesDataType = 'array';

  /**
   * @param AuthzPolicyAuthzRuleFromRequestSource[]
   */
  public function setNotSources($notSources)
  {
    $this->notSources = $notSources;
  }
  /**
   * @return AuthzPolicyAuthzRuleFromRequestSource[]
   */
  public function getNotSources()
  {
    return $this->notSources;
  }
  /**
   * @param AuthzPolicyAuthzRuleFromRequestSource[]
   */
  public function setSources($sources)
  {
    $this->sources = $sources;
  }
  /**
   * @return AuthzPolicyAuthzRuleFromRequestSource[]
   */
  public function getSources()
  {
    return $this->sources;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(AuthzPolicyAuthzRuleFrom::class, 'Google_Service_NetworkSecurity_AuthzPolicyAuthzRuleFrom');
