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

namespace Google\Service\Contactcenterinsights;

class GoogleCloudContactcenterinsightsV1AnnotatorSelector extends \Google\Collection
{
  protected $collection_key = 'phraseMatchers';
  /**
   * @var string[]
   */
  public $issueModels;
  /**
   * @var string[]
   */
  public $phraseMatchers;
  protected $qaConfigType = GoogleCloudContactcenterinsightsV1AnnotatorSelectorQaConfig::class;
  protected $qaConfigDataType = '';
  /**
   * @var bool
   */
  public $runEntityAnnotator;
  /**
   * @var bool
   */
  public $runIntentAnnotator;
  /**
   * @var bool
   */
  public $runInterruptionAnnotator;
  /**
   * @var bool
   */
  public $runIssueModelAnnotator;
  /**
   * @var bool
   */
  public $runPhraseMatcherAnnotator;
  /**
   * @var bool
   */
  public $runQaAnnotator;
  /**
   * @var bool
   */
  public $runSentimentAnnotator;
  /**
   * @var bool
   */
  public $runSilenceAnnotator;
  /**
   * @var bool
   */
  public $runSummarizationAnnotator;
  protected $summarizationConfigType = GoogleCloudContactcenterinsightsV1AnnotatorSelectorSummarizationConfig::class;
  protected $summarizationConfigDataType = '';

  /**
   * @param string[]
   */
  public function setIssueModels($issueModels)
  {
    $this->issueModels = $issueModels;
  }
  /**
   * @return string[]
   */
  public function getIssueModels()
  {
    return $this->issueModels;
  }
  /**
   * @param string[]
   */
  public function setPhraseMatchers($phraseMatchers)
  {
    $this->phraseMatchers = $phraseMatchers;
  }
  /**
   * @return string[]
   */
  public function getPhraseMatchers()
  {
    return $this->phraseMatchers;
  }
  /**
   * @param GoogleCloudContactcenterinsightsV1AnnotatorSelectorQaConfig
   */
  public function setQaConfig(GoogleCloudContactcenterinsightsV1AnnotatorSelectorQaConfig $qaConfig)
  {
    $this->qaConfig = $qaConfig;
  }
  /**
   * @return GoogleCloudContactcenterinsightsV1AnnotatorSelectorQaConfig
   */
  public function getQaConfig()
  {
    return $this->qaConfig;
  }
  /**
   * @param bool
   */
  public function setRunEntityAnnotator($runEntityAnnotator)
  {
    $this->runEntityAnnotator = $runEntityAnnotator;
  }
  /**
   * @return bool
   */
  public function getRunEntityAnnotator()
  {
    return $this->runEntityAnnotator;
  }
  /**
   * @param bool
   */
  public function setRunIntentAnnotator($runIntentAnnotator)
  {
    $this->runIntentAnnotator = $runIntentAnnotator;
  }
  /**
   * @return bool
   */
  public function getRunIntentAnnotator()
  {
    return $this->runIntentAnnotator;
  }
  /**
   * @param bool
   */
  public function setRunInterruptionAnnotator($runInterruptionAnnotator)
  {
    $this->runInterruptionAnnotator = $runInterruptionAnnotator;
  }
  /**
   * @return bool
   */
  public function getRunInterruptionAnnotator()
  {
    return $this->runInterruptionAnnotator;
  }
  /**
   * @param bool
   */
  public function setRunIssueModelAnnotator($runIssueModelAnnotator)
  {
    $this->runIssueModelAnnotator = $runIssueModelAnnotator;
  }
  /**
   * @return bool
   */
  public function getRunIssueModelAnnotator()
  {
    return $this->runIssueModelAnnotator;
  }
  /**
   * @param bool
   */
  public function setRunPhraseMatcherAnnotator($runPhraseMatcherAnnotator)
  {
    $this->runPhraseMatcherAnnotator = $runPhraseMatcherAnnotator;
  }
  /**
   * @return bool
   */
  public function getRunPhraseMatcherAnnotator()
  {
    return $this->runPhraseMatcherAnnotator;
  }
  /**
   * @param bool
   */
  public function setRunQaAnnotator($runQaAnnotator)
  {
    $this->runQaAnnotator = $runQaAnnotator;
  }
  /**
   * @return bool
   */
  public function getRunQaAnnotator()
  {
    return $this->runQaAnnotator;
  }
  /**
   * @param bool
   */
  public function setRunSentimentAnnotator($runSentimentAnnotator)
  {
    $this->runSentimentAnnotator = $runSentimentAnnotator;
  }
  /**
   * @return bool
   */
  public function getRunSentimentAnnotator()
  {
    return $this->runSentimentAnnotator;
  }
  /**
   * @param bool
   */
  public function setRunSilenceAnnotator($runSilenceAnnotator)
  {
    $this->runSilenceAnnotator = $runSilenceAnnotator;
  }
  /**
   * @return bool
   */
  public function getRunSilenceAnnotator()
  {
    return $this->runSilenceAnnotator;
  }
  /**
   * @param bool
   */
  public function setRunSummarizationAnnotator($runSummarizationAnnotator)
  {
    $this->runSummarizationAnnotator = $runSummarizationAnnotator;
  }
  /**
   * @return bool
   */
  public function getRunSummarizationAnnotator()
  {
    return $this->runSummarizationAnnotator;
  }
  /**
   * @param GoogleCloudContactcenterinsightsV1AnnotatorSelectorSummarizationConfig
   */
  public function setSummarizationConfig(GoogleCloudContactcenterinsightsV1AnnotatorSelectorSummarizationConfig $summarizationConfig)
  {
    $this->summarizationConfig = $summarizationConfig;
  }
  /**
   * @return GoogleCloudContactcenterinsightsV1AnnotatorSelectorSummarizationConfig
   */
  public function getSummarizationConfig()
  {
    return $this->summarizationConfig;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GoogleCloudContactcenterinsightsV1AnnotatorSelector::class, 'Google_Service_Contactcenterinsights_GoogleCloudContactcenterinsightsV1AnnotatorSelector');
