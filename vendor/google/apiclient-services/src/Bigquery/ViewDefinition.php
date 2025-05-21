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

namespace Google\Service\Bigquery;

class ViewDefinition extends \Google\Collection
{
  protected $collection_key = 'userDefinedFunctionResources';
  protected $foreignDefinitionsType = ForeignViewDefinition::class;
  protected $foreignDefinitionsDataType = 'array';
  protected $privacyPolicyType = PrivacyPolicy::class;
  protected $privacyPolicyDataType = '';
  /**
   * @var string
   */
  public $query;
  /**
   * @var bool
   */
  public $useExplicitColumnNames;
  /**
   * @var bool
   */
  public $useLegacySql;
  protected $userDefinedFunctionResourcesType = UserDefinedFunctionResource::class;
  protected $userDefinedFunctionResourcesDataType = 'array';

  /**
   * @param ForeignViewDefinition[]
   */
  public function setForeignDefinitions($foreignDefinitions)
  {
    $this->foreignDefinitions = $foreignDefinitions;
  }
  /**
   * @return ForeignViewDefinition[]
   */
  public function getForeignDefinitions()
  {
    return $this->foreignDefinitions;
  }
  /**
   * @param PrivacyPolicy
   */
  public function setPrivacyPolicy(PrivacyPolicy $privacyPolicy)
  {
    $this->privacyPolicy = $privacyPolicy;
  }
  /**
   * @return PrivacyPolicy
   */
  public function getPrivacyPolicy()
  {
    return $this->privacyPolicy;
  }
  /**
   * @param string
   */
  public function setQuery($query)
  {
    $this->query = $query;
  }
  /**
   * @return string
   */
  public function getQuery()
  {
    return $this->query;
  }
  /**
   * @param bool
   */
  public function setUseExplicitColumnNames($useExplicitColumnNames)
  {
    $this->useExplicitColumnNames = $useExplicitColumnNames;
  }
  /**
   * @return bool
   */
  public function getUseExplicitColumnNames()
  {
    return $this->useExplicitColumnNames;
  }
  /**
   * @param bool
   */
  public function setUseLegacySql($useLegacySql)
  {
    $this->useLegacySql = $useLegacySql;
  }
  /**
   * @return bool
   */
  public function getUseLegacySql()
  {
    return $this->useLegacySql;
  }
  /**
   * @param UserDefinedFunctionResource[]
   */
  public function setUserDefinedFunctionResources($userDefinedFunctionResources)
  {
    $this->userDefinedFunctionResources = $userDefinedFunctionResources;
  }
  /**
   * @return UserDefinedFunctionResource[]
   */
  public function getUserDefinedFunctionResources()
  {
    return $this->userDefinedFunctionResources;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(ViewDefinition::class, 'Google_Service_Bigquery_ViewDefinition');
