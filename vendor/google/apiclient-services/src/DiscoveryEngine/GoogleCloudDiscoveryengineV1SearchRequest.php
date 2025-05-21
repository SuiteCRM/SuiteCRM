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

namespace Google\Service\DiscoveryEngine;

class GoogleCloudDiscoveryengineV1SearchRequest extends \Google\Collection
{
  protected $collection_key = 'facetSpecs';
  protected $boostSpecType = GoogleCloudDiscoveryengineV1SearchRequestBoostSpec::class;
  protected $boostSpecDataType = '';
  /**
   * @var string
   */
  public $branch;
  /**
   * @var string
   */
  public $canonicalFilter;
  protected $contentSearchSpecType = GoogleCloudDiscoveryengineV1SearchRequestContentSearchSpec::class;
  protected $contentSearchSpecDataType = '';
  protected $dataStoreSpecsType = GoogleCloudDiscoveryengineV1SearchRequestDataStoreSpec::class;
  protected $dataStoreSpecsDataType = 'array';
  protected $facetSpecsType = GoogleCloudDiscoveryengineV1SearchRequestFacetSpec::class;
  protected $facetSpecsDataType = 'array';
  /**
   * @var string
   */
  public $filter;
  protected $imageQueryType = GoogleCloudDiscoveryengineV1SearchRequestImageQuery::class;
  protected $imageQueryDataType = '';
  /**
   * @var string
   */
  public $languageCode;
  /**
   * @var int
   */
  public $offset;
  /**
   * @var int
   */
  public $oneBoxPageSize;
  /**
   * @var string
   */
  public $orderBy;
  /**
   * @var int
   */
  public $pageSize;
  /**
   * @var string
   */
  public $pageToken;
  /**
   * @var array[]
   */
  public $params;
  /**
   * @var string
   */
  public $query;
  protected $queryExpansionSpecType = GoogleCloudDiscoveryengineV1SearchRequestQueryExpansionSpec::class;
  protected $queryExpansionSpecDataType = '';
  /**
   * @var bool
   */
  public $safeSearch;
  protected $searchAsYouTypeSpecType = GoogleCloudDiscoveryengineV1SearchRequestSearchAsYouTypeSpec::class;
  protected $searchAsYouTypeSpecDataType = '';
  /**
   * @var string
   */
  public $session;
  protected $sessionSpecType = GoogleCloudDiscoveryengineV1SearchRequestSessionSpec::class;
  protected $sessionSpecDataType = '';
  protected $spellCorrectionSpecType = GoogleCloudDiscoveryengineV1SearchRequestSpellCorrectionSpec::class;
  protected $spellCorrectionSpecDataType = '';
  protected $userInfoType = GoogleCloudDiscoveryengineV1UserInfo::class;
  protected $userInfoDataType = '';
  /**
   * @var string[]
   */
  public $userLabels;
  /**
   * @var string
   */
  public $userPseudoId;

  /**
   * @param GoogleCloudDiscoveryengineV1SearchRequestBoostSpec
   */
  public function setBoostSpec(GoogleCloudDiscoveryengineV1SearchRequestBoostSpec $boostSpec)
  {
    $this->boostSpec = $boostSpec;
  }
  /**
   * @return GoogleCloudDiscoveryengineV1SearchRequestBoostSpec
   */
  public function getBoostSpec()
  {
    return $this->boostSpec;
  }
  /**
   * @param string
   */
  public function setBranch($branch)
  {
    $this->branch = $branch;
  }
  /**
   * @return string
   */
  public function getBranch()
  {
    return $this->branch;
  }
  /**
   * @param string
   */
  public function setCanonicalFilter($canonicalFilter)
  {
    $this->canonicalFilter = $canonicalFilter;
  }
  /**
   * @return string
   */
  public function getCanonicalFilter()
  {
    return $this->canonicalFilter;
  }
  /**
   * @param GoogleCloudDiscoveryengineV1SearchRequestContentSearchSpec
   */
  public function setContentSearchSpec(GoogleCloudDiscoveryengineV1SearchRequestContentSearchSpec $contentSearchSpec)
  {
    $this->contentSearchSpec = $contentSearchSpec;
  }
  /**
   * @return GoogleCloudDiscoveryengineV1SearchRequestContentSearchSpec
   */
  public function getContentSearchSpec()
  {
    return $this->contentSearchSpec;
  }
  /**
   * @param GoogleCloudDiscoveryengineV1SearchRequestDataStoreSpec[]
   */
  public function setDataStoreSpecs($dataStoreSpecs)
  {
    $this->dataStoreSpecs = $dataStoreSpecs;
  }
  /**
   * @return GoogleCloudDiscoveryengineV1SearchRequestDataStoreSpec[]
   */
  public function getDataStoreSpecs()
  {
    return $this->dataStoreSpecs;
  }
  /**
   * @param GoogleCloudDiscoveryengineV1SearchRequestFacetSpec[]
   */
  public function setFacetSpecs($facetSpecs)
  {
    $this->facetSpecs = $facetSpecs;
  }
  /**
   * @return GoogleCloudDiscoveryengineV1SearchRequestFacetSpec[]
   */
  public function getFacetSpecs()
  {
    return $this->facetSpecs;
  }
  /**
   * @param string
   */
  public function setFilter($filter)
  {
    $this->filter = $filter;
  }
  /**
   * @return string
   */
  public function getFilter()
  {
    return $this->filter;
  }
  /**
   * @param GoogleCloudDiscoveryengineV1SearchRequestImageQuery
   */
  public function setImageQuery(GoogleCloudDiscoveryengineV1SearchRequestImageQuery $imageQuery)
  {
    $this->imageQuery = $imageQuery;
  }
  /**
   * @return GoogleCloudDiscoveryengineV1SearchRequestImageQuery
   */
  public function getImageQuery()
  {
    return $this->imageQuery;
  }
  /**
   * @param string
   */
  public function setLanguageCode($languageCode)
  {
    $this->languageCode = $languageCode;
  }
  /**
   * @return string
   */
  public function getLanguageCode()
  {
    return $this->languageCode;
  }
  /**
   * @param int
   */
  public function setOffset($offset)
  {
    $this->offset = $offset;
  }
  /**
   * @return int
   */
  public function getOffset()
  {
    return $this->offset;
  }
  /**
   * @param int
   */
  public function setOneBoxPageSize($oneBoxPageSize)
  {
    $this->oneBoxPageSize = $oneBoxPageSize;
  }
  /**
   * @return int
   */
  public function getOneBoxPageSize()
  {
    return $this->oneBoxPageSize;
  }
  /**
   * @param string
   */
  public function setOrderBy($orderBy)
  {
    $this->orderBy = $orderBy;
  }
  /**
   * @return string
   */
  public function getOrderBy()
  {
    return $this->orderBy;
  }
  /**
   * @param int
   */
  public function setPageSize($pageSize)
  {
    $this->pageSize = $pageSize;
  }
  /**
   * @return int
   */
  public function getPageSize()
  {
    return $this->pageSize;
  }
  /**
   * @param string
   */
  public function setPageToken($pageToken)
  {
    $this->pageToken = $pageToken;
  }
  /**
   * @return string
   */
  public function getPageToken()
  {
    return $this->pageToken;
  }
  /**
   * @param array[]
   */
  public function setParams($params)
  {
    $this->params = $params;
  }
  /**
   * @return array[]
   */
  public function getParams()
  {
    return $this->params;
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
   * @param GoogleCloudDiscoveryengineV1SearchRequestQueryExpansionSpec
   */
  public function setQueryExpansionSpec(GoogleCloudDiscoveryengineV1SearchRequestQueryExpansionSpec $queryExpansionSpec)
  {
    $this->queryExpansionSpec = $queryExpansionSpec;
  }
  /**
   * @return GoogleCloudDiscoveryengineV1SearchRequestQueryExpansionSpec
   */
  public function getQueryExpansionSpec()
  {
    return $this->queryExpansionSpec;
  }
  /**
   * @param bool
   */
  public function setSafeSearch($safeSearch)
  {
    $this->safeSearch = $safeSearch;
  }
  /**
   * @return bool
   */
  public function getSafeSearch()
  {
    return $this->safeSearch;
  }
  /**
   * @param GoogleCloudDiscoveryengineV1SearchRequestSearchAsYouTypeSpec
   */
  public function setSearchAsYouTypeSpec(GoogleCloudDiscoveryengineV1SearchRequestSearchAsYouTypeSpec $searchAsYouTypeSpec)
  {
    $this->searchAsYouTypeSpec = $searchAsYouTypeSpec;
  }
  /**
   * @return GoogleCloudDiscoveryengineV1SearchRequestSearchAsYouTypeSpec
   */
  public function getSearchAsYouTypeSpec()
  {
    return $this->searchAsYouTypeSpec;
  }
  /**
   * @param string
   */
  public function setSession($session)
  {
    $this->session = $session;
  }
  /**
   * @return string
   */
  public function getSession()
  {
    return $this->session;
  }
  /**
   * @param GoogleCloudDiscoveryengineV1SearchRequestSessionSpec
   */
  public function setSessionSpec(GoogleCloudDiscoveryengineV1SearchRequestSessionSpec $sessionSpec)
  {
    $this->sessionSpec = $sessionSpec;
  }
  /**
   * @return GoogleCloudDiscoveryengineV1SearchRequestSessionSpec
   */
  public function getSessionSpec()
  {
    return $this->sessionSpec;
  }
  /**
   * @param GoogleCloudDiscoveryengineV1SearchRequestSpellCorrectionSpec
   */
  public function setSpellCorrectionSpec(GoogleCloudDiscoveryengineV1SearchRequestSpellCorrectionSpec $spellCorrectionSpec)
  {
    $this->spellCorrectionSpec = $spellCorrectionSpec;
  }
  /**
   * @return GoogleCloudDiscoveryengineV1SearchRequestSpellCorrectionSpec
   */
  public function getSpellCorrectionSpec()
  {
    return $this->spellCorrectionSpec;
  }
  /**
   * @param GoogleCloudDiscoveryengineV1UserInfo
   */
  public function setUserInfo(GoogleCloudDiscoveryengineV1UserInfo $userInfo)
  {
    $this->userInfo = $userInfo;
  }
  /**
   * @return GoogleCloudDiscoveryengineV1UserInfo
   */
  public function getUserInfo()
  {
    return $this->userInfo;
  }
  /**
   * @param string[]
   */
  public function setUserLabels($userLabels)
  {
    $this->userLabels = $userLabels;
  }
  /**
   * @return string[]
   */
  public function getUserLabels()
  {
    return $this->userLabels;
  }
  /**
   * @param string
   */
  public function setUserPseudoId($userPseudoId)
  {
    $this->userPseudoId = $userPseudoId;
  }
  /**
   * @return string
   */
  public function getUserPseudoId()
  {
    return $this->userPseudoId;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GoogleCloudDiscoveryengineV1SearchRequest::class, 'Google_Service_DiscoveryEngine_GoogleCloudDiscoveryengineV1SearchRequest');
