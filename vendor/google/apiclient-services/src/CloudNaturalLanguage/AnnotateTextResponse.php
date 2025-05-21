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

namespace Google\Service\CloudNaturalLanguage;

class AnnotateTextResponse extends \Google\Collection
{
  protected $collection_key = 'sentences';
  protected $categoriesType = ClassificationCategory::class;
  protected $categoriesDataType = 'array';
  protected $documentSentimentType = Sentiment::class;
  protected $documentSentimentDataType = '';
  protected $entitiesType = Entity::class;
  protected $entitiesDataType = 'array';
  /**
   * @var string
   */
  public $languageCode;
  /**
   * @var bool
   */
  public $languageSupported;
  protected $moderationCategoriesType = ClassificationCategory::class;
  protected $moderationCategoriesDataType = 'array';
  protected $sentencesType = Sentence::class;
  protected $sentencesDataType = 'array';

  /**
   * @param ClassificationCategory[]
   */
  public function setCategories($categories)
  {
    $this->categories = $categories;
  }
  /**
   * @return ClassificationCategory[]
   */
  public function getCategories()
  {
    return $this->categories;
  }
  /**
   * @param Sentiment
   */
  public function setDocumentSentiment(Sentiment $documentSentiment)
  {
    $this->documentSentiment = $documentSentiment;
  }
  /**
   * @return Sentiment
   */
  public function getDocumentSentiment()
  {
    return $this->documentSentiment;
  }
  /**
   * @param Entity[]
   */
  public function setEntities($entities)
  {
    $this->entities = $entities;
  }
  /**
   * @return Entity[]
   */
  public function getEntities()
  {
    return $this->entities;
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
   * @param bool
   */
  public function setLanguageSupported($languageSupported)
  {
    $this->languageSupported = $languageSupported;
  }
  /**
   * @return bool
   */
  public function getLanguageSupported()
  {
    return $this->languageSupported;
  }
  /**
   * @param ClassificationCategory[]
   */
  public function setModerationCategories($moderationCategories)
  {
    $this->moderationCategories = $moderationCategories;
  }
  /**
   * @return ClassificationCategory[]
   */
  public function getModerationCategories()
  {
    return $this->moderationCategories;
  }
  /**
   * @param Sentence[]
   */
  public function setSentences($sentences)
  {
    $this->sentences = $sentences;
  }
  /**
   * @return Sentence[]
   */
  public function getSentences()
  {
    return $this->sentences;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(AnnotateTextResponse::class, 'Google_Service_CloudNaturalLanguage_AnnotateTextResponse');
