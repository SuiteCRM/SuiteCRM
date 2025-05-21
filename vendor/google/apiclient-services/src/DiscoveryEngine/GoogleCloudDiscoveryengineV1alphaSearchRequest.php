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

class GoogleCloudDiscoveryengineV1alphaSearchRequest extends \Google\Collection
{
  protected $collection_key = 'facetSpecs';
  protected $boostSpecType = GoogleCloudDiscoveryengineV1alphaSearchRequestBoostSpec::class;
  protected $boostSpecDataType = '';
  /**
   * @var string
   */
  public $branch;
  /**
   * @var string
   */
  public $canonicalFilter;
  protected $contentSearchSpecType = GoogleCloudDiscoveryengineV1alphaSearchRequestContentSearchSpec::class;
  protected $contentSearchSpecDataType = '';
  protected $customFineTuningSpecType = GoogleCloudDiscoveryengineV1alphaCustomFineTuningSpec::class;
  protected $customFineTuningSpecDataType = '';
  protected $dataStoreSpecsType = GoogleCloudDiscoveryengineV1alphaSearchRequestDataStoreSpec::class;
  protected $dataStoreSpecsDataType = 'array';
  protected $displaySpecType = GoogleCloudDiscoveryengineV1alphaSearchRequestDisplaySpec::class;
  protected $displaySpecDataType = '';
  protected $embeddingSpecType = GoogleCloudDiscoveryengineV1alphaSearchRequestEmbeddingSpec::class;
  protected $embeddingSpecDataType = '';
  protected $facetSpecsType = GoogleCloudDiscoveryengineV1alphaSearchRequestFacetSpec::class;
  protected $facetSpecsDataType = 'array';
  /**
   * @var string
   */
  public $filter;
  protected $imageQueryType = GoogleCloudDiscoveryengineV1alphaSearchRequestImageQuery::class;
  protected $imageQueryDataType = '';
  /**
   * @var string
   */
  public $languageCode;
  protected $naturalLanguageQueryUnderstandingSpecType = GoogleCloudDiscoveryengineV1alphaSearchRequestNaturalLanguageQueryUnderstandingSpec::class;
  protected $naturalLanguageQueryUnderstandingSpecDataType = '';
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
  protected $personalizationSpecType = GoogleCloudDiscoveryengineV1alphaSearchRequestPersonalizationSpec::class;
  protected $personalizationSpecDataType = '';
  /**
   * @var string
   */
  public $query;
  protected $queryExpansionSpecType = GoogleCloudDiscoveryengineV1alphaSearchRequestQueryExpansionSpec::class;
  protected $queryExpansionSpecDataType = '';
  /**
   * @var string
   */
  public $rankingExpression;
  /**
   * @var string
   */
  public $rankingExpressionBackend;
  /**
   * @var string
   */
  public $regionCode;
  /**
   * @var string
   */
  public $relevanceThreshold;
  /**
   * @var bool
   */
  public $safeSearch;
  protected $searchAsYouTypeSpecType = GoogleCloudDiscoveryengineV1alphaSearchRequestSearchAsYouTypeSpec::class;
  protected $searchAsYouTypeSpecDataType = '';
  /**
   * @var string
   */
  public $servingConfig;
  /**
   * @var string
   */
  public $session;
  protected $sessionSpecType = GoogleCloudDiscoveryengineV1alphaSearchRequestSessionSpec::class;
  protected $sessionSpecDataType = '';
  protected $spellCorrectionSpecType = GoogleCloudDiscoveryengineV1alphaSearchRequestSpellCorrectionSpec::class;
  protected $spellCorrectionSpecDataType = '';
  protected $userInfoType = GoogleCloudDiscoveryengineV1alphaUserInfo::class;
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
   * @param GoogleCloudDiscoveryengineV1alphaSearchRequestBoostSpec
   */
  public function setBoostSpec(GoogleCloudDiscoveryengineV1alphaSearchRequestBoostSpec $boostSpec)
  {
    $this->boostSpec = $boostSpec;
  }
  /**
   * @return GoogleCloudDiscoveryengineV1alphaSearchRequestBoostSpec
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
   * @param GoogleCloudDiscoveryengineV1alphaSearchRequestContentSearchSpec
   */
  public function setContentSearchSpec(GoogleCloudDiscoveryengineV1alphaSearchRequestContentSearchSpec $contentSearchSpec)
  {
    $this->contentSearchSpec = $contentSearchSpec;
  }
  /**
   * @return GoogleCloudDiscoveryengineV1alphaSearchRequestContentSearchSpec
   */
  public function getContentSearchSpec()
  {
    return $this->contentSearchSpec;
  }
  /**
   * @param GoogleCloudDiscoveryengineV1alphaCustomFineTuningSpec
   */
  public function setCustomFineTuningSpec(GoogleCloudDiscoveryengineV1alphaCustomFineTuningSpec $customFineTuningSpec)
  {
    $this->customFineTuningSpec = $customFineTuningSpec;
  }
  /**
   * @return GoogleCloudDiscoveryengineV1alphaCustomFineTuningSpec
   */
  public function getCustomFineTuningSpec()
  {
    return $this->customFineTuningSpec;
  }
  /**
   * @param GoogleCloudDiscoveryengineV1alphaSearchRequestDataStoreSpec[]
   */
  public function setDataStoreSpecs($dataStoreSpecs)
  {
    $this->dataStoreSpecs = $dataStoreSpecs;
  }
  /**
   * @return GoogleCloudDiscoveryengineV1alphaSearchRequestDataStoreSpec[]
   */
  public function getDataStoreSpecs()
  {
    return $this->dataStoreSpecs;
  }
  /**
   * @param GoogleCloudDiscoveryengineV1alphaSearchRequestDisplaySpec
   */
  public function setDisplaySpec(GoogleCloudDiscoveryengineV1alphaSearchRequestDisplaySpec $displaySpec)
  {
    $this->displaySpec = $displaySpec;
  }
  /**
   * @return GoogleCloudDiscoveryengineV1alphaSearchRequestDisplaySpec
   */
  public function getDisplaySpec()
  {
    return $this->displaySpec;
  }
  /**
   * @param GoogleCloudDiscoveryengineV1alphaSearchRequestEmbeddingSpec
   */
  public function setEmbeddingSpec(GoogleCloudDiscoveryengineV1alphaSearchRequestEmbeddingSpec $embeddingSpec)
  {
    $this->embeddingSpec = $embeddingSpec;
  }
  /**
   * @return GoogleCloudDiscoveryengineV1alphaSearchRequestEmbeddingSpec
   */
  public function getEmbeddingSpec()
  {
    return $this->embeddingSpec;
  }
  /**
   * @param GoogleCloudDiscoveryengineV1alphaSearchRequestFacetSpec[]
   */
  public function setFacetSpecs($facetSpecs)
  {
    $this->facetSpecs = $facetSpecs;
  }
  /**
   * @return GoogleCloudDiscoveryengineV1alphaSearchRequestFacetSpec[]
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
   * @param GoogleCloudDiscoveryengineV1alphaSearchRequestImageQuery
   */
  public function setImageQuery(GoogleCloudDiscoveryengineV1alphaSearchRequestImageQuery $imageQuery)
  {
    $this->imageQuery = $imageQuery;
  }
  /**
   * @return GoogleCloudDiscoveryengineV1alphaSearchRequestImageQuery
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
   * @param GoogleCloudDiscoveryengineV1alphaSearchRequestNaturalLanguageQueryUnderstandingSpec
   */
  public function setNaturalLanguageQueryUnderstandingSpec(GoogleCloudDiscoveryengineV1alphaSearchRequestNaturalLanguageQueryUnderstandingSpec $naturalLanguageQueryUnderstandingSpec)
  {
    $this->naturalLanguageQueryUnderstandingSpec = $naturalLanguageQueryUnderstandingSpec;
  }
  /**
   * @return GoogleCloudDiscoveryengineV1alphaSearchRequestNaturalLanguageQueryUnderstandingSpec
   */
  public function getNaturalLanguageQueryUnderstandingSpec()
  {
    return $this->naturalLanguageQueryUnderstandingSpec;
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
   * @param GoogleCloudDiscoveryengineV1alphaSearchRequestPersonalizationSpec
   */
  public function setPersonalizationSpec(GoogleCloudDiscoveryengineV1alphaSearchRequestPersonalizationSpec $personalizationSpec)
  {
    $this->personalizationSpec = $personalizationSpec;
  }
  /**
   * @return GoogleCloudDiscoveryengineV1alphaSearchRequestPersonalizationSpec
   */
  public function getPersonalizationSpec()
  {
    return $this->personalizationSpec;
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
   * @param GoogleCloudDiscoveryengineV1alphaSearchRequestQueryExpansionSpec
   */
  public function setQueryExpansionSpec(GoogleCloudDiscoveryengineV1alphaSearchRequestQueryExpansionSpec $queryExpansionSpec)
  {
    $this->queryExpansionSpec = $queryExpansionSpec;
  }
  /**
   * @return GoogleCloudDiscoveryengineV1alphaSearchRequestQueryExpansionSpec
   */
  public function getQueryExpansionSpec()
  {
    return $this->queryExpansionSpec;
  }
  /**
   * @param string
   */
  public function setRankingExpression($rankingExpression)
  {
    $this->rankingExpression = $rankingExpression;
  }
  /**
   * @return string
   */
  public function getRankingExpression()
  {
    return $this->rankingExpression;
  }
  /**
   * @param string
   */
  public function setRankingExpressionBackend($rankingExpressionBackend)
  {
    $this->rankingExpressionBackend = $rankingExpressionBackend;
  }
  /**
   * @return string
   */
  public function getRankingExpressionBackend()
  {
    return $this->rankingExpressionBackend;
  }
  /**
   * @param string
   */
  public function setRegionCode($regionCode)
  {
    $this->regionCode = $regionCode;
  }
  /**
   * @return string
   */
  public function getRegionCode()
  {
    return $this->regionCode;
  }
  /**
   * @param string
   */
  public function setRelevanceThreshold($relevanceThreshold)
  {
    $this->relevanceThreshold = $relevanceThreshold;
  }
  /**
   * @return string
   */
  public function getRelevanceThreshold()
  {
    return $this->relevanceThreshold;
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
   * @param GoogleCloudDiscoveryengineV1alphaSearchRequestSearchAsYouTypeSpec
   */
  public function setSearchAsYouTypeSpec(GoogleCloudDiscoveryengineV1alphaSearchRequestSearchAsYouTypeSpec $searchAsYouTypeSpec)
  {
    $this->searchAsYouTypeSpec = $searchAsYouTypeSpec;
  }
  /**
   * @return GoogleCloudDiscoveryengineV1alphaSearchRequestSearchAsYouTypeSpec
   */
  public function getSearchAsYouTypeSpec()
  {
    return $this->searchAsYouTypeSpec;
  }
  /**
   * @param string
   */
  public function setServingConfig($servingConfig)
  {
    $this->servingConfig = $servingConfig;
  }
  /**
   * @return string
   */
  public function getServingConfig()
  {
    return $this->servingConfig;
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
   * @param GoogleCloudDiscoveryengineV1alphaSearchRequestSessionSpec
   */
  public function setSessionSpec(GoogleCloudDiscoveryengineV1alphaSearchRequestSessionSpec $sessionSpec)
  {
    $this->sessionSpec = $sessionSpec;
  }
  /**
   * @return GoogleCloudDiscoveryengineV1alphaSearchRequestSessionSpec
   */
  public function getSessionSpec()
  {
    return $this->sessionSpec;
  }
  /**
   * @param GoogleCloudDiscoveryengineV1alphaSearchRequestSpellCorrectionSpec
   */
  public function setSpellCorrectionSpec(GoogleCloudDiscoveryengineV1alphaSearchRequestSpellCorrectionSpec $spellCorrectionSpec)
  {
    $this->spellCorrectionSpec = $spellCorrectionSpec;
  }
  /**
   * @return GoogleCloudDiscoveryengineV1alphaSearchRequestSpellCorrectionSpec
   */
  public function getSpellCorrectionSpec()
  {
    return $this->spellCorrectionSpec;
  }
  /**
   * @param GoogleCloudDiscoveryengineV1alphaUserInfo
   */
  public function setUserInfo(GoogleCloudDiscoveryengineV1alphaUserInfo $userInfo)
  {
    $this->userInfo = $userInfo;
  }
  /**
   * @return GoogleCloudDiscoveryengineV1alphaUserInfo
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
class_alias(GoogleCloudDiscoveryengineV1alphaSearchRequest::class, 'Google_Service_DiscoveryEngine_GoogleCloudDiscoveryengineV1alphaSearchRequest');
